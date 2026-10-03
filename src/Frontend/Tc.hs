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
import Frontend.Builtin qualified as Builtins
import Frontend.Error
import Frontend.Renamer.Types
import Frontend.Substitution (Substitution)
import Frontend.Substitution qualified as Sub
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
import Frontend.Typechecker.Polytype (PolyType (PolyType), instantiate)

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

getFuns :: (Data a) => a -> [(Ident, (PolyType Tc, SourceInfo))]
getFuns = listify' f
  where
    f :: FnRn -> Maybe (Ident, (PolyType Tc, SourceInfo))
    f (Fn info name (Params _ tyvars) args returnType _) =
        let funTy = PolyType tyvars (TyFun NoExtField (fmap typeOf args) (typeOf returnType))
         in Just (name, (funTy, info))

data TypeCons = TypeCons
    { types :: [(Ident, (PolyType Tc, SourceInfo))]
    , cons :: [(Ident, (PolyType Tc, SourceInfo))]
    }

getTypesAndCons :: (Data a) => a -> TypeCons
getTypesAndCons a = TypeCons {types = listify' h a, cons = concat (listify' f a)}
  where
    h :: AdtRn -> Maybe (Ident, (PolyType Tc, SourceInfo))
    -- TODO: Add type params to polytype
    h (Adt loc name _) = Just (name, (PolyType [] $ TyCon NoExtField name, loc))

    f :: AdtRn -> Maybe [(Ident, (PolyType Tc, SourceInfo))]
    f (Adt _ name cons) =
        -- TODO: Add type params to polytype
        let returnType = TyCon NoExtField name
         in Just $ fmap (g returnType) cons
      where
        g :: TypeTc -> ConstructorRn -> (Ident, (PolyType Tc, SourceInfo))
        g returnType = \case
            EnumCons loc name -> (name, (PolyType [] $ returnType, loc))
            FunCons loc name argTys ->
                (name, (PolyType [] $ TyFun NoExtField (fmap typeOf argTys) returnType, loc))

tc ::
    DefTable Tc (PolyType Tc) SourceInfo -> Names -> ProgramRn -> (Either [TcError] ProgramTc, [TcWarning])
tc defTable names (Program _namespace defs) =
    case first partitionEithers $ unzip $ fmap (tcDefs names defTable) defs of
        (([], defs), warnings) -> (Right $ Program NoExtField defs, mconcat warnings)
        ((errs, _), warnings) -> (Left $ mconcat errs, mconcat warnings)

tcDefs ::
    Names ->
    DefTable Tc (PolyType Tc) SourceInfo ->
    DefRn ->
    (Either [TcError] DefTc, [TcWarning])
tcDefs names table (DefFn fn) =
    first (fmap DefFn) $ tcFunction names table fn
tcDefs _ _ (DefAdt adt) = first (Right . DefAdt) $ tcAdt adt
tcDefs _ table (DefImport imp) = (Right (DefImport (tcImport table imp)), [])

tcImport :: DefTable Tc (PolyType Tc) SourceInfo -> ImportRn -> ImportTc
tcImport table (ImportExplicit _ namespace names) =
    let funs :: Map Ident (PolyType Tc, SourceInfo)
        funs =
            fromMaybe
                ( error
                    $ "Failed finding namespace `"
                    <> show namespace
                    <> "` in table: "
                    <> show (view functions table)
                )
                $ Map.lookup namespace (view functions table)
        tys :: [(PolyType Tc, SourceInfo)]
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
        mkFnType :: (PolyType Tc, SourceInfo) -> FnType
        mkFnType (ty, _) = case ty of
            PolyType tvars (TyFun _ args ret) -> FnType tvars ret args
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
    DefTable Tc (PolyType Tc) SourceInfo ->
    FnRn ->
    (Either [TcError] FnTc, [TcWarning])
tcFunction names defTable fun@(Fn _ _ tyParams args rt _) =
    let argTable =
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
        env = Env argTable
     in run ctx env $ go fun
  where
    go :: Fn Rn -> TcM (Fn Tc)
    go (Fn loc name tyParams args rt block) =
        locally Ctx.currentFun (const fun) $ do
            args <- mapM infArg args
            let retTy = typeOf rt
            -- unify @_ @TypeTc loc (TyLit NoExtField Unit) retTy -- TODO: only for main
            block <- locally Ctx.returnType (const retTy) $ case block of
                Block info stmts (Just expr) -> do
                    stmts <- mapM infStmt stmts
                    expr <- tcExpr retTy expr
                    pure $ Block (info, retTy) stmts (Just expr)
                Block info stmts Nothing -> do
                    stmts <- mapM infStmt stmts
                    pure $ Block (info, Any) stmts Nothing
            pure (Fn NoExtField name tyParams args retTy block)

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

infArg :: (Monad m) => ArgRn -> m ArgTc
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
            Toplevel -> first instantiate <$> lookupFun namespace name
            Constructor -> first instantiate <$> lookupCon namespace name
            Imported -> first instantiate <$> lookupFun namespace name
            Builtin -> do
                builtins <- view (defTable . builtIns)
                case Builtins.lookup namespace name builtins of
                    Just (ty, res) -> pure (ty, res)
                    _ -> error "INTERNAL ERROR: Missing builtin"
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
    App info lExpr rExprs -> do
        lExpr <- infExpr lExpr
        let tcApp ty = case ty of
                TyFun NoExtField argTys retTy -> do
                    let argTysLength = length argTys
                    let rLength = length rExprs
                    if
                        | argTysLength < rLength -> do
                            retTy <-
                                Any
                                    <$ tooManyArguments
                                        info
                                        argTysLength
                                        rLength
                            r <- mapM infExpr rExprs
                            pure $ App (info, retTy) lExpr r
                        | argTysLength > rLength -> do
                            retTy <-
                                Any
                                    <$ partiallyAppliedFunction
                                        info
                                        argTysLength
                                        rLength
                            r <- mapM infExpr rExprs
                            pure $ App (info, retTy) lExpr r
                        | otherwise -> do
                            rExprs <- mapM infExpr rExprs
                            zipWithM_ (unify info) argTys rExprs
                            pure (App (info, retTy) lExpr rExprs)
                ty -> do
                    retTy <- Any <$ applyNonFunction info ty
                    r <- mapM infExpr rExprs
                    pure $ App (info, retTy) lExpr r
        tcApp (typeOf lExpr)
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
                ty <- maybe (Any <$ typeMustBeKnown' info name) pure ty'
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

unifyArgs ::
    Map TyVar (Maybe TypeTc) -> [(TypeTc, ExprRn)] -> TcM ([ExprTc], Map TyVar (Maybe TypeTc))
unifyArgs tyvarMap [] = pure ([], tyvarMap)
unifyArgs tyvarMap ((ty, expr) : xs) = do
    (m1, expr) <- unifyArg tyvarMap ty expr
    (exprs, m2) <- unifyArgs m1 xs
    pure (expr : exprs, Map.union m1 m2)

unifyArg ::
    Map TyVar (Maybe TypeTc) ->
    TypeTc ->
    ExprRn ->
    TcM (Map TyVar (Maybe TypeTc), ExprTc)
unifyArg tyvarMap argType expr = case argType of
    TypeVar bound tyvar -> do
        case Map.lookup tyvar tyvarMap of
            Just Nothing -> do
                expr <- infExpr expr
                pure (Map.insert tyvar (Just $ typeOf expr) tyvarMap, expr)
            Just (Just ty) -> (tyvarMap,) <$> tcExpr ty expr
            Nothing -> (tyvarMap,) <$> tcExpr argType expr
    _ -> (tyvarMap,) <$> tcExpr argType expr

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
        (ty, _declLoc) <- first instantiate <$> lookupCon namespace conName
        unify' loc pattype ty
        pure $ PEnumCon (loc, ty) conName
    PFunCon (loc, namespace) conName pats -> do
        (ty, _declLoc) <- first instantiate <$> lookupCon namespace conName
        case ty of
            TyFun tyParamList argtys retty
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
        -- TODO: Do actual typechecking otherwise lambdas always have to be annotated.
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
            TyFun tyParamList argtys retty
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

lookupCon :: (MonadReader Ctx m) => Namespace -> Ident -> m (PolyType Tc, SourceInfo)
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

lookupFun :: (MonadReader Ctx m) => Namespace -> Ident -> m (PolyType Tc, SourceInfo)
lookupFun namespace name =
    views
        (Ctx.defTable . DefTable.functions)
        ( fromMaybe (error ("INTERNAL ERROR: Unable to find name: " <> show name))
            . (Map.lookup name <=< Map.lookup namespace)
        )

class TypeOf a where
    typeOf :: a -> TypeTc

instance TypeOf TypeTc where
    typeOf ty = ty

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
        TyLit NoExtField b -> TyLit NoExtField b
        TyFun NoExtField b c -> TyFun NoExtField (fmap typeOf b) (typeOf c) -- NOTE: is `emptyTyParamList` correct?
        TyCon NoExtField b -> TyCon NoExtField b
        TypeVar NoExtField b -> TypeVar NoExtField b

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

-- | Unify two types. The first argument type *must* be the expected one!
unify' ::
    (MonadValidate [TcError] m, MonadReader Ctx m) =>
    SourceInfo ->
    TypeTc ->
    TypeTc ->
    m ()
unify' info ty1 ty2 = case (ty1, ty2) of
    (TyLit _ lit1, TyLit _ lit2)
        | lit1 == lit2 -> pure ()
        | otherwise -> void $ tyExpectedGot info [ty1] ty2
    (TypeVar _ tvar1, TypeVar _ tvar2)
        | tvar1 == tvar2 -> pure ()
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

unifiesSubst :: SourceInfo -> TyParamList -> [(TypeTc, TypeTc)] -> Maybe (Substitution Tc)
unifiesSubst loc params = foldlM f Sub.empty
  where
    f :: Substitution Tc -> (TypeTc, TypeTc) -> Maybe (Substitution Tc)
    f sub (ty1, ty2) = Sub.compose sub =<< unifySubst loc params ty1 ty2

-- If ty1 is a type variable and it is a member of TyParamList then it will be unified with the second type
unifySubst :: SourceInfo -> TyParamList -> TypeTc -> TypeTc -> Maybe (Substitution Tc)
unifySubst loc params ty1 ty2 = case (ty1, ty2) of
    (TyLit _ lit1, TyLit _ lit2)
        | lit1 == lit2 -> pure Sub.empty
        | otherwise -> Nothing
    (TypeVar _ tvar1, TypeVar _ tvar2)
        | tvar1 == tvar2 -> pure Sub.empty
        | tvar1 `member` params -> pure (Sub.singleton tvar1 ty2)
        | otherwise -> Nothing
    (TypeVar _ tvar1, ty2)
        | tvar1 `member` params -> pure (Sub.singleton tvar1 ty2)
        | otherwise -> Nothing
    (TyFun _ l1 r1, TyFun _ l2 r2) ->
        case length l1 == length l2 of
            False -> Nothing
            True -> do
                argsSub <-
                    foldlM (\sub (lty, rty) -> Sub.compose <$> sub <*> unifySubst loc params lty rty) (Just Sub.empty)
                        $ zip l1 l2
                retSub <- unifySubst loc params r1 r2
                argsSub <- argsSub
                Sub.compose argsSub retSub
    (TyCon NoExtField name1, TyCon NoExtField name2)
        | name1 == name2 -> pure Sub.empty
        | otherwise -> Nothing
    (Type AnyX, _) -> pure Sub.empty
    _ -> Nothing
