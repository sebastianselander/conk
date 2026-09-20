{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Frontend.Tc where

import Control.Lens.Getter (uses, view, views)
import Control.Lens.Setter (locally, modifying)
import Control.Lens.TH
import Control.Monad.Validate (MonadValidate, ValidateT, runValidateT)
import Control.Monad.Writer (Writer, runWriter)
import Data.Data (Data)
import Data.Map.Strict qualified as Map
import Frontend.Error
import Frontend.Renamer.Types
import Frontend.Typechecker.Ctx (Ctx, defTable)
import Frontend.Typechecker.Ctx qualified as Ctx
import Frontend.Typechecker.Types
import Frontend.Types
import Names (Ident, Names, Namespace, getOriginalName')
import Relude hiding (Any, Type, intercalate)
import Relude.Unsafe (fromJust)
import Table (DefTable, builtIns, functions)
import Table qualified as DefTable
import Utils (chain, listify')

newtype Env = Env
    { _variables :: Map Ident (TypeTc, SourceInfo)
    }
    deriving (Show)

$(makeLenses ''Env)
newtype TcM a = Tc
    { runTc ::
        StateT Env (ReaderT Ctx (ValidateT [TcError] (Writer [TcWarning]))) a
    }
    deriving
        ( Functor
        , Applicative
        , Monad
        , MonadReader Ctx
        , MonadValidate [TcError]
        , MonadState Env
        )

run :: Ctx -> Env -> TcM a -> (Either [TcError] a, [TcWarning])
run ctx env = runWriter . runValidateT . flip runReaderT ctx . flip evalStateT env . runTc

getFuns :: (Data a) => a -> [(Ident, (TypeTc, SourceInfo))]
getFuns = listify' f
  where
    f :: FnRn -> Maybe (Ident, (TypeTc, SourceInfo))
    f (Fn info name args returnType _) =
        let funTy = TyFun NoExtField (fmap typeOf args) (typeOf returnType)
         in Just (name, (funTy, info))

data TypeCons = TypeCons
    { types :: [(Ident, (TypeTc, SourceInfo))]
    , cons :: [(Ident, (TypeTc, SourceInfo))]
    }

getTypesAndCons :: (Data a) => a -> TypeCons
getTypesAndCons a = TypeCons {types = listify' h a, cons = concat (listify' f a)}
  where
    h :: AdtRn -> Maybe (Ident, (TypeTc, SourceInfo))
    h (Adt loc name _) = Just (name, (TyCon NoExtField name, loc))
    f :: AdtRn -> Maybe [(Ident, (TypeTc, SourceInfo))]
    f (Adt _ name cons) =
        let returnType = TyCon NoExtField name
         in Just $ fmap (g returnType) cons
      where
        g :: TypeTc -> ConstructorRn -> (Ident, (TypeTc, SourceInfo))
        g returnType = \case
            EnumCons loc name -> (name, (returnType, loc))
            FunCons loc name argTys ->
                (name, (TyFun NoExtField (fmap typeOf argTys) returnType, loc))

tc ::
    DefTable TypeTc SourceInfo -> Names -> ProgramRn -> (Either [TcError] ProgramTc, [TcWarning])
tc defTable names (Program _namespace defs) =
    case first partitionEithers $ unzip $ fmap (tcDefs names defTable) defs of
        (([], defs), warnings) -> (Right $ Program NoExtField defs, mconcat warnings)
        ((errs, _), warnings) -> (Left $ mconcat errs, mconcat warnings)

tcDefs ::
    Names ->
    DefTable TypeTc SourceInfo ->
    DefRn ->
    (Either [TcError] DefTc, [TcWarning])
tcDefs names table (DefFn fn) =
    first (fmap DefFn) $ tcFunction names table fn
tcDefs _ _ (DefAdt adt) = first (Right . DefAdt) $ tcAdt adt
tcDefs _ table (DefImport imp) = (Right (DefImport (tcImport table imp)), [])

tcImport :: DefTable TypeTc SourceInfo -> ImportRn -> ImportTc
tcImport table (ImportExplicit _ namespace names) =
    let funs :: Map Ident (TypeTc, SourceInfo)
        funs =
            fromMaybe
                ( error
                    $ "Failed finding namespace `"
                    <> show namespace
                    <> "` in table: "
                    <> show (view functions table)
                )
                $ Map.lookup namespace (view functions table)
        tys :: [(TypeTc, SourceInfo)]
        tys =
            fmap
                ( \symbol ->
                    ( \x ->
                        fromMaybe
                            ( error
                                $ "Failed finding symbol `"
                                <> show symbol
                                <> "`in imported program: "
                                <> show x
                            )
                            x
                    )
                        (Map.lookup symbol funs)
                )
                names
        mkFnType :: (TypeTc, SourceInfo) -> FnType
        mkFnType (ty, _) = case ty of
            TyFun _ args ret -> FnType ret args
            ty -> error $ "Imported symbol is not a function: " <> show ty
     in ImportExplicit (fmap mkFnType tys) namespace names

tcAdt :: AdtRn -> (AdtTc, [TcWarning])
tcAdt (Adt loc name constructors) =
    (Adt loc name (fmap (inferConstructor (TyCon NoExtField name)) constructors), [])

inferConstructor :: TypeTc -> ConstructorRn -> ConstructorTc
inferConstructor ty = \case
    EnumCons loc name -> EnumCons (loc, ty) name
    FunCons loc name types ->
        let types' = fmap typeOf types
         in FunCons (loc, TyFun NoExtField types' ty) name types'

tcFunction ::
    Names ->
    DefTable TypeTc SourceInfo ->
    FnRn ->
    (Either [TcError] FnTc, [TcWarning])
tcFunction names defTable fun@(Fn _ _ args rt _) =
    let varTable =
            foldr
                ( uncurry Map.insert
                    . ( \(Arg info name ty) ->
                            ( name
                            ,
                                ( typeOf ty
                                , fst info
                                )
                            )
                      )
                )
                mempty
                args
        ctx = Ctx.Ctx defTable (typeOf rt) fun [] names
        env = Env varTable
     in run ctx env $ go fun
  where
    go (Fn _ name args rt block) =
        locally Ctx.currentFun (const fun) $ do
            args <- mapM infArg args
            let retTy = typeOf rt
            block <- locally Ctx.returnType (const retTy) $ case block of
                Block info stmts (Just expr) -> do
                    stmts <- mapM infStmt stmts
                    expr <- tcExpr retTy expr
                    pure $ Block (info, retTy) stmts (Just expr)
                Block info stmts Nothing -> do
                    stmts <- mapM infStmt stmts
                    pure $ Block (info, Any) stmts Nothing
            pure (Fn NoExtField name args retTy block)

inferBlock :: BlockRn -> TcM BlockTc
inferBlock (Block info statements tailExpression) = do
    stmts <- mapM infStmt statements
    expr <- mapM infExpr tailExpression
    pure $ Block (info, maybe (TyLit NoExtField Unit) typeOf expr) stmts expr

tcBlock :: TypeTc -> Block Rn -> TcM BlockTc
tcBlock expectedTy (Block info statements tailExpression) = do
    stmts <- mapM infStmt statements
    expr <- case tailExpression of
        Nothing -> do
            unless
                (expectedTy == TyLit NoExtField Unit)
                (tyExpectedGot info [expectedTy] (TyLit NoExtField Unit))
            pure Nothing
        Just tail -> Just <$> tcExpr expectedTy tail
    pure $ Block (info, maybe (TyLit NoExtField Unit) typeOf expr) stmts expr

infArg :: ArgRn -> TcM ArgTc
infArg (Arg _ name ty) = pure $ Arg NoExtField name (typeOf ty)

infStmt :: StmtRn -> TcM StmtTc
infStmt (SExpr NoExtField expr) = SExpr NoExtField <$> infExpr expr

breaks :: BlockTc -> [ExprTc]
breaks (Block _ stmts tail) =
    concatMap (\(SExpr NoExtField e) -> breakExpr e) stmts
        <> maybe [] breakExpr tail
  where
    -- \| Find all breaks in a block. Do not traverse further on expressions where breaks are allowed
    breakExpr :: ExprTc -> [ExprTc]
    breakExpr e = case e of
        Lit {} -> []
        Var {} -> []
        Break {} -> [e]
        If _ cond l r -> breakExpr cond <> breaks l <> maybe [] breaks r
        BinOp _ l _ r -> breakExpr l <> breakExpr r
        Prefix _ _ e -> breakExpr e
        App _ l rs -> breakExpr l <> concatMap breakExpr rs
        Let _ _ expr -> breakExpr expr
        Ass _ _ _ expr -> breakExpr expr
        Ret _ expr -> maybe [] breakExpr expr
        EBlock _ block -> breaks block
        While {} -> []
        Loop {} -> []
        Lam {} -> []
        Match _ scrutinee arms -> breakExpr scrutinee <> concatMap breakArm arms
      where
        breakArm :: MatchArm Tc -> [ExprTc]
        breakArm (MatchArm _ _ body) = breakExpr body

infExpr :: ExprRn -> TcM ExprTc
infExpr currentExpr = Ctx.push currentExpr $ case currentExpr of
    Lit info lit ->
        let (ty, b) = infLit lit
         in pure $ Lit (info, ty) b
    Var (info, namespace, boundedness) name -> do
        (ty, _declaredAtInfo) <- case boundedness of
            Free -> lookupVar name
            Bound -> lookupVar name
            Toplevel -> lookupFun namespace name
            Constructor -> lookupCon namespace name
            Imported -> lookupFun namespace name
            Builtin -> do
                builtins <- view (defTable . builtIns)
                case Map.lookup name =<< Map.lookup namespace builtins of
                    Just res -> pure res
                    Nothing -> error "INTERNAL ERROR: Missing builtin"
        pure $ Var (info, ty, boundedness) name
    Prefix info Neg expr -> do
        expr <- tcExpr (TyLit NoExtField Int) expr
        pure $ Prefix (info, TyLit NoExtField Int) Neg expr
    Prefix info Not expr -> do
        expr <- tcExpr (TyLit NoExtField Bool) expr
        pure $ Prefix (info, TyLit NoExtField Bool) Neg expr
    BinOp info l op r -> do
        let typeOfOp = operatorType op
        l <- tcExpr typeOfOp l
        r <- tcExpr typeOfOp r
        let retty = operatorReturnType (operatorType op) op
        pure $ BinOp (info, retty) l op r
    App info l r -> do
        l <- infExpr l
        let tcApp ty = case ty of
                TyFun NoExtField argTys retTy -> do
                    let argTysLength = length argTys
                    let rLength = length r
                    if
                        | argTysLength < rLength -> do
                            retTy <-
                                Any
                                    <$ tooManyArguments
                                        info
                                        argTysLength
                                        rLength
                            r <- mapM infExpr r
                            pure $ App (info, retTy) l r
                        | argTysLength > rLength -> do
                            retTy <-
                                Any
                                    <$ partiallyAppliedFunction
                                        info
                                        argTysLength
                                        rLength
                            r <- mapM infExpr r
                            pure $ App (info, retTy) l r
                        | otherwise -> do
                            -- invariant: argTys and r are of equal length
                            r <- zipWithM tcExpr argTys r
                            pure $ App (info, retTy) l r
                ty -> do
                    retTy <- Any <$ applyNonFunction info ty
                    r <- mapM infExpr r
                    pure $ App (info, retTy) l r
        tcApp (typeOf l)
    Let (info, mbty) name expr -> do
        expr <- maybe (infExpr expr) ((`tcExpr` expr) . typeOf) mbty
        let ty = typeOf expr
        insertVar name ty info
        pure $ Let (StmtType (TyLit NoExtField Unit) ty info) name expr
    Ass (info, bind, _namespace) name op expr -> do
        (ty, info) <- case bind of
            Toplevel ->
                assignNonVariable @TcM info
                    <$> views Ctx.names (getOriginalName' name)
                    >> pure (Any, info)
            _ -> lookupVar name
        expr <- tcExpr ty expr
        unify info ty expr
        ty <- pure ty
        case op of
            AddAssign ->
                unless
                    (ty `elem` [TyLit NoExtField Int, TyLit NoExtField Double])
                    (void $ tyExpectedGot info [TyLit NoExtField Int, TyLit NoExtField Double] ty)
            SubAssign ->
                unless
                    (ty `elem` [TyLit NoExtField Int, TyLit NoExtField Double])
                    (void $ tyExpectedGot info [TyLit NoExtField Int, TyLit NoExtField Double] ty)
            MulAssign ->
                unless
                    (ty `elem` [TyLit NoExtField Int, TyLit NoExtField Double])
                    (void $ tyExpectedGot info [TyLit NoExtField Int, TyLit NoExtField Double] ty)
            DivAssign ->
                unless
                    (ty `elem` [TyLit NoExtField Int, TyLit NoExtField Double])
                    (void $ tyExpectedGot info [TyLit NoExtField Int, TyLit NoExtField Double] ty)
            ModAssign ->
                unless
                    (ty `elem` [TyLit NoExtField Int, TyLit NoExtField Double])
                    (void $ tyExpectedGot info [TyLit NoExtField Int, TyLit NoExtField Double] ty)
            Assign -> pure ()
        pure $ Ass (StmtType (TyLit NoExtField Unit) ty info, bind) name op expr
    Ret info mbExpr -> do
        returnType <- view Ctx.returnType
        case mbExpr of
            Nothing -> do
                unless (returnType == TyLit NoExtField Unit) (void $ emptyReturnNonUnit info returnType)
                pure $ Ret (info, TyLit NoExtField Unit) Nothing
            Just expr -> do
                expr <- tcExpr returnType expr
                pure $ Ret (info, returnType) (Just expr)
    EBlock NoExtField block -> EBlock NoExtField <$> inferBlock block
    Break info expr -> do
        expr <- mapM infExpr expr
        pure $ Break (info, maybe (TyLit NoExtField Unit) typeOf expr) expr
    If info condition true false -> do
        condition <- tcExpr (TyLit NoExtField Bool) condition
        true <- inferBlock true
        let ty = typeOf true
        false <- mapM (tcBlock ty) false
        pure $ If (info, ty) condition true false
    While info expr block -> do
        expr <- tcExpr (TyLit NoExtField Bool) expr
        block <- tcBlock (TyLit NoExtField Unit) block
        case block of
            block@(Block _ _ tail) -> do
                let breakExprs = breaks block
                maybe (pure ()) (unify info (TyLit NoExtField Unit)) tail
                mapM_ (\expr -> unify (hasInfo expr) (TyLit NoExtField Unit) expr) breakExprs
        pure $ While (info, TyLit NoExtField Unit) expr block
    Loop info block -> do
        block <- tcBlock (TyLit NoExtField Unit) block
        ty <- case breaks block of
            [] -> pure Any
            (x : xs) -> do
                sequence_
                    $ chain
                        (\expr1 expr2 -> unify (hasInfo expr2) (typeOf expr1) expr2)
                        x
                        xs
                pure (typeOf x)
        pure $ Loop (info, ty) block
    Lam info args body -> do
        let insertArg (LamArg (info, ty, _namespace) name) = do
                let ty' = fmap typeOf ty
                ty <- maybe (Any <$ typeMustBeKnown info name) pure ty'
                insertVar name ty info
                pure $ LamArg ty name
        args <- mapM insertArg args
        body <- infExpr body
        let ty = TyFun NoExtField (fmap typeOf args) (typeOf body)
        pure $ Lam (info, ty) args body
    Match loc scrutinee matchArms -> do
        scrutinee <- infExpr scrutinee
        let scrutType = typeOf scrutinee
        case matchArms of
            [] -> pure $ Match (loc, Any) scrutinee []
            (arm1 : arms) -> do
                arm1 <- infMatchArm scrutType arm1
                let armType = typeOf arm1
                arms <- mapM (tcMatchArm scrutType armType) arms
                pure $ Match (loc, armType) scrutinee (arm1 : arms)

infMatchArm :: TypeTc -> MatchArmRn -> TcM MatchArmTc
infMatchArm pattype (MatchArm loc pat body) = do
    pat <- tcPat pattype pat
    body <- infExpr body
    pure $ MatchArm loc pat body

tcMatchArm :: TypeTc -> TypeTc -> MatchArmRn -> TcM MatchArmTc
tcMatchArm pattype bodytype (MatchArm loc pat body) = do
    pat <- tcPat pattype pat
    body <- tcExpr bodytype body
    pure $ MatchArm loc pat body

tcPat :: TypeTc -> PatternRn -> TcM PatternTc
tcPat pattype currentPattern = case currentPattern of
    PVar loc varName -> do
        insertVar varName pattype loc
        pure $ PVar (loc, pattype) varName
    PEnumCon (loc, namespace) conName -> do
        (ty, _declLoc) <- lookupCon namespace conName
        unify' loc pattype ty
        pure $ PEnumCon (loc, ty) conName
    PFunCon (loc, namespace) conName pats -> do
        (ty, _declLoc) <- lookupCon namespace conName
        case ty of
            TyFun NoExtField argtys retty
                | length argtys == length pats -> do
                    unify' loc pattype retty
                    pats <- zipWithM tcPat argtys pats
                    pure $ PFunCon (loc, pattype) conName pats
                | otherwise -> do
                    expectedPatNArgs loc currentPattern (length argtys) (length pats)
                    pats <- zipWithM tcPat (repeat Any) pats
                    pure $ PFunCon (loc, pattype) conName pats
            other -> do
                tyExpectedGot loc [pattype] other
                pats <- zipWithM tcPat (repeat Any) pats
                pure $ PFunCon (loc, pattype) conName pats

tcExpr :: TypeTc -> ExprRn -> TcM ExprTc
tcExpr expectedTy currentExpr = Ctx.push currentExpr $ case currentExpr of
    Lit info lit -> do
        let (ty, lit') = infLit lit
        let literal = Lit (info, ty) lit'
        void $ unify info expectedTy literal
        pure literal
    Var (info, _namespace, _) _ -> do
        expr <- infExpr currentExpr
        unify info expectedTy expr
        pure expr
    Prefix info op expr -> do
        case op of
            Not -> do
                expr <- tcExpr (TyLit NoExtField Bool) expr
                let expr' = Prefix (info, TyLit NoExtField Bool) op expr
                unify info expectedTy expr'
                pure expr'
            Neg -> do
                expr <- infExpr expr
                let ty = typeOf expr
                unless
                    (ty `elem` [TyLit NoExtField Int, TyLit NoExtField Double])
                    (tyExpectedGot info [TyLit NoExtField Int, TyLit NoExtField Double] ty)
                pure $ Prefix (info, ty) op expr
    BinOp info l op r -> do
        let typeOfOp = operatorType op
        l <- tcExpr typeOfOp l
        r <- tcExpr typeOfOp r
        let retty = operatorReturnType (operatorType op) op
        let expr = BinOp (info, retty) l op r
        unify info expectedTy expr
        pure expr
    App info fun args -> do
        expr <- infExpr (App info fun args)
        unify info expectedTy expr
        pure expr
    Let (info, mbty) name expr -> do
        expr <- maybe (infExpr expr) ((`tcExpr` expr) . typeOf) mbty
        let ty = typeOf expr
        unify' info expectedTy (TyLit NoExtField Unit)
        insertVar name ty info
        pure $ Let (StmtType (TyLit NoExtField Unit) ty info) name expr
    Ass (info, bind, namespace) name op expr -> do
        expr <- infExpr (Ass (info, bind, namespace) name op expr)
        unify info expectedTy expr
        pure expr
    Ret info expr -> do
        returnType <- view Ctx.returnType
        case expr of
            Nothing -> do
                unify' info returnType (TyLit NoExtField Unit)
                pure $ Ret (info, Any) Nothing
            Just expr -> do
                expr <- tcExpr returnType expr
                pure $ Ret (info, Any) (Just expr)
    EBlock NoExtField block -> do
        block <- tcBlock expectedTy block
        pure $ EBlock NoExtField block
    Break info expr -> do
        expr <- mapM infExpr expr
        pure $ Break (info, maybe (TyLit NoExtField Unit) typeOf expr) expr
    If info cond true false -> do
        cond <- tcExpr (TyLit NoExtField Bool) cond
        true <- tcBlock expectedTy true
        false <- mapM (tcBlock expectedTy) false
        pure $ If (info, expectedTy) cond true false
    while@(While info _ _) -> do
        while <- infExpr while
        unify info expectedTy while
        pure while
    Loop info block -> do
        block <- tcBlock expectedTy block
        mapM_ (unify info expectedTy) (breaks block)
        pure $ Loop (info, expectedTy) block
    Lam info args body -> do
        case expectedTy of
            TyFun NoExtField argtys retty
                | length argtys == length args -> do
                    lamArgs <- unifyLambdaArgs (zip argtys args)
                    body <- tcExpr retty body
                    pure $ Lam (info, expectedTy) lamArgs body
                | otherwise -> expectedLambdaNArgs' info (length argtys) (length args)
            _ -> expectedTyGotLambda' info expectedTy
    Match loc scrutinee matchArms -> do
        scrutinee <- infExpr scrutinee
        let scrutType = typeOf scrutinee
        arms <- mapM (tcMatchArm scrutType expectedTy) matchArms
        pure $ Match (loc, expectedTy) scrutinee arms

-- | Unify the arguments of a lambda with the the given types
unifyLambdaArgs ::
    (MonadReader Ctx m, MonadValidate [TcError] m, MonadState Env m) =>
    [(TypeTc, LamArgRn)] ->
    m [LamArgTc]
unifyLambdaArgs [] = pure []
unifyLambdaArgs
    ((expectedType, LamArg (loc, mbArgumentType, _namespace) argumentName) : xs) = do
        mapM_ (unify loc expectedType) mbArgumentType
        let (LamArg ty name) = LamArg @Tc expectedType argumentName
        insertVar name ty loc
        rest <- unifyLambdaArgs xs
        pure (LamArg ty name : rest)

hasInfo :: ExprTc -> SourceInfo
hasInfo = \case
    Lit info _ -> fst info
    Var (info, _, _) _ -> info
    Prefix info _ _ -> fst info
    BinOp info _ _ _ -> fst info
    App info _ _ -> fst info
    Let info _ _ -> view stmtInfo info
    Ass (info, _) _ _ _ -> view stmtInfo info
    Ret info _ -> fst info
    EBlock _ (Block info _ _) -> fst info
    Break info _ -> fst info
    If info _ _ _ -> fst info
    While info _ _ -> fst info
    Loop info _ -> fst info
    Lam info _ _ -> fst info
    Match info _ _ -> fst info

operatorReturnType :: TypeTc -> BinOp -> TypeTc
operatorReturnType inputTy = \case
    Mul -> inputTy
    Div -> inputTy
    Add -> inputTy
    Sub -> inputTy
    Mod -> inputTy
    Or -> TyLit NoExtField Bool
    And -> TyLit NoExtField Bool
    Lt -> TyLit NoExtField Bool
    Gt -> TyLit NoExtField Bool
    Lte -> TyLit NoExtField Bool
    Gte -> TyLit NoExtField Bool
    Eq -> TyLit NoExtField Bool
    Neq -> TyLit NoExtField Bool

operatorType :: BinOp -> TypeTc
operatorType = \case
    Mul -> TyLit NoExtField Int
    Div -> TyLit NoExtField Int
    Add -> TyLit NoExtField Int
    Sub -> TyLit NoExtField Int
    Mod -> TyLit NoExtField Int
    Or -> TyLit NoExtField Bool
    And -> TyLit NoExtField Bool
    Lt -> TyLit NoExtField Int
    Gt -> TyLit NoExtField Int
    Lte -> TyLit NoExtField Int
    Gte -> TyLit NoExtField Int
    Eq -> TyLit NoExtField Int
    Neq -> TyLit NoExtField Int

infLit :: LitRn -> (TypeTc, LitTc)
infLit = \case
    IntLit info n -> (TyLit NoExtField Int, IntLit info n)
    DoubleLit info n -> (TyLit NoExtField Double, DoubleLit info n)
    StringLit info s -> (TyLit NoExtField String, StringLit info s)
    CharLit info c -> (TyLit NoExtField Char, CharLit info c)
    BoolLit info b -> (TyLit NoExtField Bool, BoolLit info b)
    UnitLit info -> (TyLit NoExtField Unit, UnitLit info)

insertVar :: (MonadState Env m) => Ident -> TypeTc -> SourceInfo -> m ()
insertVar name ty info = modifying variables (Map.insert name (ty, info))

lookupVar :: (MonadState Env m) => Ident -> m (TypeTc, SourceInfo)
lookupVar name =
    uses
        variables
        ( fromMaybe (error $ "INTERNAL ERROR: Could not find variable: " <> show name)
            . Map.lookup name
        )

lookupCon :: (MonadReader Ctx m) => Namespace -> Ident -> m (TypeTc, SourceInfo)
lookupCon namespace name =
    views
        Ctx.defTable
        ( fromJust
            . Map.lookup name
            . fromJust
            . Map.lookup namespace
            . view DefTable.constructors
        )

lookupVarTy :: (MonadState Env m) => Ident -> m TypeTc
lookupVarTy = fmap fst . lookupVar

lookupFun :: (MonadReader Ctx m) => Namespace -> Ident -> m (TypeTc, SourceInfo)
lookupFun namespace name =
    views
        (Ctx.defTable . DefTable.functions)
        ( fromMaybe (error ("INTERNAL ERROR: Unable to find name: " <> show name))
            . (Map.lookup name <=< Map.lookup namespace)
        )

class TypeOf a where
    typeOf :: a -> TypeTc

instance TypeOf StmtTc where
    typeOf = \case
        SExpr _ expr -> typeOf expr

instance TypeOf ExprTc where
    typeOf = \case
        Lit ty _ -> snd ty
        Var (_, ty, _) _ -> ty
        Prefix ty _ _ -> snd ty
        BinOp ty _ _ _ -> snd ty
        App ty _ _ -> snd ty
        Let rec _ _ -> view stmtType rec
        Ass (rec, _) _ _ _ -> view stmtType rec
        Ret ty _ -> snd ty
        EBlock _ block -> typeOf block
        Break ty _ -> snd ty
        If ty _ _ _ -> snd ty
        While ty _ _ -> snd ty
        Loop ty _ -> snd ty
        Lam ty _ _ -> snd ty
        Match ty _ _ -> snd ty

instance TypeOf BlockTc where
    typeOf (Block ty _ _) = snd ty

instance TypeOf TypeRn where
    typeOf = \case
        TyLit a b -> TyLit a b
        TyFun a b c -> TyFun a (fmap typeOf b) (typeOf c)
        TyCon a b -> TyCon a b

instance TypeOf LamArgTc where
    typeOf (LamArg ty _) = ty

instance TypeOf MatchArmTc where
    typeOf (MatchArm _ _ body) = typeOf body

instance TypeOf ArgTc where
    typeOf (Arg _ _ ty) = ty

instance TypeOf ArgRn where
    typeOf (Arg _ _ ty) = typeOf ty

unify ::
    (MonadReader Ctx m, MonadState Env m, MonadValidate [TcError] m, TypeOf a) =>
    SourceInfo ->
    TypeTc ->
    a ->
    m ()
unify info ty1 a = unify' info ty1 (typeOf a)

-- | Unify two types. The first argument type *must* the expected one!
unify' ::
    (MonadState Env m, MonadValidate [TcError] m, MonadReader Ctx m) =>
    SourceInfo ->
    TypeTc ->
    TypeTc ->
    m ()
unify' info ty1 ty2 = case (ty1, ty2) of
    (TyLit _ lit1, TyLit _ lit2)
        | lit1 == lit2 -> pure ()
        | otherwise -> void $ tyExpectedGot info [ty1] ty2
    (TyFun _ l1 r1, TyFun _ l2 r2) -> do
        unless (length l1 == length l2) (void $ tyExpectedGot info [ty1] ty2)
        zipWithM_ (unify' info) l1 l2
        unify' info r1 r2
    (Type AnyX, _) -> pure ()
    (_, Type AnyX) -> pure ()
    (TyCon NoExtField name1, TyCon NoExtField name2)
        | name1 == name2 -> pure ()
        | otherwise -> tyExpectedGot info [ty1] ty2
    (ty1, ty2) -> void $ tyExpectedGot info [ty1] ty2
