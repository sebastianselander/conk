{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Frontend.Tc where

import Control.Lens (use)
import Control.Lens.Getter (uses, view, views)
import Control.Lens.Setter (assign, locally, modifying, (+=))
import Control.Lens.TH
import Control.Monad.Validate (MonadValidate, ValidateT, runValidateT)
import Control.Monad.Writer (Writer, runWriter)
import Data.Data (Data)
import Relude hiding (Any, Type, intercalate)
import Relude.Unsafe (fromJust)

import Data.Map.Strict qualified as Map

import Frontend.Error
import Frontend.Renamer.Types
import Frontend.Substitution (Substitute (apply))
import Frontend.Typechecker.Ctx (Ctx, defTable)
import Frontend.Typechecker.Polytype (instantiate)
import Frontend.Typechecker.Types
import Frontend.Typechecker.Unify (typeOf, unifies, unify)
import Frontend.Types
import Names (Ident, Names, Namespace, getOriginalName')
import Table (DefTable, builtIns, functions)
import Utils (listify')

import Frontend.Builtin qualified as Builtins
import Frontend.Typechecker.Ctx qualified as Ctx
import Table qualified as DefTable


data Env = Env
    { _variables :: Map Ident (PolyType Tc, SourceInfo)
    , _fresh :: Int
    }
    deriving (Show)


$(makeLenses ''Env)


newtype TcM a = Tc
    { runTc ::
        StateT Env (ReaderT Ctx (ValidateT [TcError] (Writer [TcWarning]))) a
    }
    deriving
        ( Applicative
        , Functor
        , Monad
        , MonadReader Ctx
        , MonadState Env
        , MonadValidate [TcError]
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
            EnumCons loc name -> (name, (PolyType [] returnType, loc))
            FunCons loc name argTys ->
                (name, (PolyType [] $ TyFun NoExtField (fmap typeOf argTys) returnType, loc))


tc ::
    DefTable Tc (PolyType Tc) SourceInfo ->
    Names ->
    ProgramRn ->
    (Either [TcError] ProgramTc, [TcWarning])
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
tcFunction names defTable fun@(Fn _ _ _ args rt _) =
    let argTable =
            foldr
                ( uncurry Map.insert
                    . ( \(Arg info name ty) ->
                            ( name
                            ,
                                ( PolyType [] $ typeOf ty -- NOTE: Is empty list correct?
                                , fst info
                                )
                            )
                      )
                )
                mempty
                args
        ctx = Ctx.Ctx defTable (typeOf rt) fun [] names
        env = Env argTable 1
     in run ctx env $ go fun
  where
    go :: Fn Rn -> TcM (Fn Tc)
    go (Fn _ name tyParams args rt block) =
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


instantiateTc :: (MonadState Env m) => PolyType Tc -> m TypeTc
instantiateTc ty = do
    fr <- use fresh
    let (ty', fr') = instantiate fr ty
    assign fresh fr'
    pure ty'


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
                case Builtins.lookup namespace name builtins of
                    Just (ty, res) -> pure (ty, res)
                    _ -> error "INTERNAL ERROR: Missing builtin"
        ty <- instantiateTc ty
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
                            sub <- unifies info (zip argTys (fmap typeOf rExprs))
                            pure $ apply sub (App (info, retTy) lExpr rExprs)
                ty -> do
                    retTy <- Any <$ applyNonFunction info ty
                    r <- mapM infExpr rExprs
                    pure $ App (info, retTy) lExpr r
        tcApp (typeOf lExpr)
    Let (info, mbty) name expr -> do
        expr <- maybe (infExpr expr) ((`tcExpr` expr) . typeOf) mbty
        let ty = typeOf expr
        insertVar name (PolyType [] ty) info
        pure $ Let (StmtType (TyLit NoExtField Unit) (PolyType [] ty) info) name expr
    Ass (info, bind, _namespace) name op expr -> do
        (ty, info) <- case bind of
            Toplevel ->
                assignNonVariable @TcM info
                    <$> views Ctx.names (getOriginalName' name)
                    >> pure (PolyType [] Any, info)
            _ -> do
                (ty, loc) <- lookupVar name
                pure (ty, loc)
        let PolyType tvars ty' = ty -- NOTE: Perhaps not correct
        expr <- tcExpr ty' expr
        sub <- unify info ty' (typeOf expr)
        ty <- pure ty
        case op of
            AddAssign ->
                unless
                    (ty' `elem` [TyLit NoExtField Int, TyLit NoExtField Double])
                    (void $ tyExpectedGot info [TyLit NoExtField Int, TyLit NoExtField Double] ty')
            SubAssign ->
                unless
                    (ty' `elem` [TyLit NoExtField Int, TyLit NoExtField Double])
                    (void $ tyExpectedGot info [TyLit NoExtField Int, TyLit NoExtField Double] ty')
            MulAssign ->
                unless
                    (ty' `elem` [TyLit NoExtField Int, TyLit NoExtField Double])
                    (void $ tyExpectedGot info [TyLit NoExtField Int, TyLit NoExtField Double] ty')
            DivAssign ->
                unless
                    (ty' `elem` [TyLit NoExtField Int, TyLit NoExtField Double])
                    (void $ tyExpectedGot info [TyLit NoExtField Int, TyLit NoExtField Double] ty')
            ModAssign ->
                unless
                    (ty' `elem` [TyLit NoExtField Int, TyLit NoExtField Double])
                    (void $ tyExpectedGot info [TyLit NoExtField Int, TyLit NoExtField Double] ty')
            Assign -> pure ()
        pure $ apply sub $ Ass (StmtType (TyLit NoExtField Unit) ty info, bind) name op expr
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
                case tail of
                    Nothing -> pure ()
                    Just tail -> do
                        let unit = TyLit NoExtField Unit
                        ensureTypesEqual (hasInfo tail) unit (typeOf tail)
                        mapM_ (\expr -> ensureTypesEqual (hasInfo expr) unit $ typeOf expr) breakExprs

        pure $ While (info, TyLit NoExtField Unit) expr block
    Loop info block -> do
        block <- tcBlock (TyLit NoExtField Unit) block
        ty <- case breaks block of
            [] -> pure Any
            (x : xs) -> do
                mapM_ (\expr -> ensureTypesEqual (hasInfo expr) (typeOf x) (typeOf expr)) xs
                pure (typeOf x)
        pure $ Loop (info, ty) block
    Lam info args body -> do
        let insertArg :: (MonadState Env m) => LamArgRn -> m LamArgTc
            insertArg (LamArg (info, ty, _namespace) name) = do
                ty <- case ty of
                    Nothing -> do
                        n <- use fresh
                        fresh += n
                        pure $ Type (Mono (MonoType n))
                    Just ty -> pure $ typeOf ty
                insertVar name (PolyType [] ty) info
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


ensureTypesEqual ::
    (MonadReader Ctx m, MonadValidate [TcError] m) => SourceInfo -> TypeTc -> TypeTc -> m ()
ensureTypesEqual loc expected got = unless (expected ~~ got) $ tyExpectedGot loc [expected] got


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
        insertVar varName (PolyType [] pattype) loc
        pure $ PVar (loc, pattype) varName
    PEnumCon (_, namespace) conName -> do
        (ty, loc) <- lookupCon namespace conName
        ty <- instantiateTc ty
        sub <- unify loc pattype ty
        pure $ apply sub $ PEnumCon (loc, ty) conName
    PFunCon (_, namespace) conName pats -> do
        (ty, loc) <- lookupCon namespace conName
        ty <- instantiateTc ty
        case ty of
            TyFun _ argtys retty
                | length argtys == length pats -> do
                    sub <- unify loc pattype retty
                    pats <- zipWithM tcPat (apply sub <$> argtys) pats
                    pure $ PFunCon (loc, apply sub pattype) conName pats
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
        _ <- unify info expectedTy ty
        pure literal
    Var (info, _namespace, binding) name -> do
        (PolyType _ ty, loc) <- lookupVar name
        sub <- unify info expectedTy ty
        pure (Var (info, apply sub ty, binding) name)
    Prefix info op expr -> do
        case op of
            Not -> do
                expr <- tcExpr (TyLit NoExtField Bool) expr
                let expr' = Prefix (info, TyLit NoExtField Bool) op expr
                ensureTypesEqual info expectedTy (typeOf expr')
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
        ensureTypesEqual info expectedTy (typeOf expr)
        pure expr
    App info fun args -> do
        expr <- infExpr (App info fun args)
        ensureTypesEqual info expectedTy (typeOf expr)
        pure expr
    Let (info, mbty) name expr -> do
        expr <- maybe (infExpr expr) ((`tcExpr` expr) . typeOf) mbty
        let ty = typeOf expr
        ensureTypesEqual info expectedTy (TyLit NoExtField Unit)
        insertVar name (PolyType [] ty) info
        pure $ Let (StmtType (TyLit NoExtField Unit) (PolyType [] ty) info) name expr
    Ass (info, bind, namespace) name op expr -> do
        expr <- infExpr (Ass (info, bind, namespace) name op expr)
        ensureTypesEqual info expectedTy (typeOf expr)
        pure expr
    Ret info expr -> do
        returnType <- view Ctx.returnType
        case expr of
            Nothing -> do
                ensureTypesEqual info returnType (TyLit NoExtField Unit)
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
        ensureTypesEqual info expectedTy (typeOf while)
        pure while
    Loop info block -> do
        block <- tcBlock expectedTy block
        mapM_ (ensureTypesEqual info expectedTy . typeOf) (breaks block)
        pure $ Loop (info, expectedTy) block
    Lam info args body -> do
        case expectedTy of
            TyFun _ argtys retty
                | length argtys == length args -> do
                    lamArgs <- tcLambdaArgs (zip argtys args)
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
tcLambdaArgs ::
    (MonadReader Ctx m, MonadValidate [TcError] m, MonadState Env m) =>
    [(TypeTc, LamArgRn)] ->
    m [LamArgTc]
tcLambdaArgs [] = pure []
tcLambdaArgs ((expected, LamArg (loc, mty, _namespace) name) : xs) = do
    arg <- case mty of
        Just argTy -> do
            ensureTypesEqual loc expected (typeOf argTy)
            pure $ LamArg (typeOf argTy) name
        Nothing -> do
            insertVar name (PolyType [] expected) loc
            pure $ LamArg expected name
    args <- tcLambdaArgs xs
    pure (arg : args)


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


insertVar :: (MonadState Env m) => Ident -> PolyType Tc -> SourceInfo -> m ()
insertVar name ty info = modifying variables (Map.insert name (ty, info))


lookupVar :: (MonadState Env m) => Ident -> m (PolyType Tc, SourceInfo)
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


lookupFun :: (MonadReader Ctx m) => Namespace -> Ident -> m (PolyType Tc, SourceInfo)
lookupFun namespace name =
    views
        (Ctx.defTable . DefTable.functions)
        ( fromMaybe (error ("INTERNAL ERROR: Unable to find name: " <> show name))
            . (Map.lookup name <=< Map.lookup namespace)
        )
