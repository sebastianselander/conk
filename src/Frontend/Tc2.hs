{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Use mapAndUnzipM" #-}

module Frontend.Tc2 where

import Control.Lens (use)
import Control.Lens.Getter (uses, view, views)
import Control.Lens.Setter (assign, locally, modifying, (+=))
import Control.Lens.TH
import Control.Monad (foldM)
import Control.Monad.Validate (MonadValidate, ValidateT, runValidateT)
import Control.Monad.Writer (Writer, runWriter)
import Data.Data (Data)
import Relude hiding (Any, Type, intercalate)
import Relude.Unsafe (fromJust)

import Data.Map.Strict qualified as Map

import Frontend.Error
import Frontend.Renamer.Types
import Frontend.Substitution (Substitute (apply), Substitution)
import Frontend.Typechecker.Ctx (Ctx, defTable)
import Frontend.Typechecker.Polytype (instantiate, instantiate_with)
import Frontend.Typechecker.Types
import Frontend.Typechecker.Unify (typeOf, unify)
import Frontend.Types
import Impossible (__IMPOSSIBLE__)
import Names (Ident, Names, Namespace, getOriginalName')
import Table (DefTable, builtIns, functions)
import Utils (listify')

import Frontend.Builtin qualified as Builtins
import Frontend.Substitution qualified as Sub
import Frontend.Typechecker.Ctx qualified as Ctx
import Table qualified as DefTable


data Env = Env
    { _variables :: Map Ident (Type Tc, SourceInfo)
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


typecheck ::
    DefTable Tc (PolyType Tc) SourceInfo ->
    Names ->
    ProgramRn ->
    (Either [TcError] ProgramTc, [TcWarning])
typecheck defTable names (Program _namespace defs) =
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
                __IMPOSSIBLE__
                $ Map.lookup namespace (view functions table)
        tys :: [(PolyType Tc, SourceInfo)]
        tys =
            fmap
                ( \symbol ->
                    fromMaybe
                        __IMPOSSIBLE__
                        (Map.lookup symbol funs)
                )
                names
        mkFnType :: (PolyType Tc, SourceInfo) -> FnType
        mkFnType (ty, _) = case ty of
            PolyType tvars (TyFun _ args ret) -> FnType tvars ret args
            _ -> __IMPOSSIBLE__
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
                                ( typeOf ty -- NOTE: Is empty list correct?
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
            args <- mapM infer_arg args
            let retTy = typeOf rt
            -- unify @_ @TypeTc loc (TyLit NoExtField Unit) retTy -- TODO: only for main
            block <- locally Ctx.returnType (const retTy) $ case block of
                Block info stmts (Just expr) -> do
                    -- NOTE: Not sure if using mapM is viable
                    (subs, stmts) <- unzip <$> mapM infer_stmt stmts
                    let sub1 = foldr Sub.compose Sub.empty subs
                    (sub2, expr) <- check_expr retTy expr
                    pure $ apply (Sub.compose sub2 sub1) $ Block (info, retTy) stmts (Just expr)
                Block info stmts Nothing -> do
                    (subs, stmts) <- unzip <$> mapM infer_stmt stmts
                    let sub = foldr Sub.compose Sub.empty subs
                    pure $ apply sub $ Block (info, Any) stmts Nothing
            pure (Fn NoExtField name tyParams args retTy block)


infer_arg :: (Monad m) => ArgRn -> m ArgTc
infer_arg (Arg _ name ty) = pure $ Arg NoExtField name (typeOf ty)


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


check_expr ::
    (MonadReader Ctx m, MonadState Env m, MonadValidate [TcError] m) =>
    TypeTc -> ExprRn -> m (Substitution Tc, ExprTc)
check_expr expected_type current_expr = Ctx.push current_expr $ case current_expr of
    Lit info lit -> do
        (sub1, lit) <- infer_lit info lit
        sub2 <- unify info expected_type (typeOf lit)
        pure (Sub.compose sub2 sub1, lit)
    Var (info, namespace, boundedness) name -> do
        (sub1, expr) <- infer_var info namespace boundedness name
        sub2 <- unify info expected_type (typeOf expr)
        let sub3 = Sub.compose sub2 sub1
        modifying variables (Map.map (first (apply sub3)))
        pure (sub3, apply sub3 expr)
    Prefix info op expr -> do
        (sub1, expr) <- infer_prefix info op expr
        sub2 <- unify info expected_type (typeOf expr)
        let sub3 = Sub.compose sub2 sub1
        modifying variables (Map.map (first (apply sub3)))
        pure (sub3, expr)
    BinOp info left op right -> do
        (sub1, expr) <- infer_binop info left op right
        sub2 <- unify info expected_type (typeOf expr)
        let sub3 = Sub.compose sub2 sub1
        modifying variables (Map.map (first (apply sub3)))
        pure (sub3, expr)
    App info lExpr rExprs -> do
        (sub1, expr) <- infer_app info lExpr rExprs
        sub2 <- unify info expected_type (typeOf expr)
        let sub3 = Sub.compose sub2 sub1
        modifying variables (Map.map (first (apply sub3)))
        pure (sub3, expr)
    Let (info, mbty) name expr -> do
        (sub1, expr) <- infer_let info mbty name expr
        sub2 <- unify info expected_type (typeOf expr)
        pure (Sub.compose sub2 sub1, expr)
    Ass (info, bind, namespace) name op expr -> do
        (sub1, expr) <- infer_ass info bind namespace name op expr
        sub2 <- unify info expected_type (typeOf expr)
        pure (Sub.compose sub2 sub1, expr)
    Ret info maybe_expr -> do
        (sub1, expr) <- infer_ret info maybe_expr
        sub2 <- unify info expected_type (typeOf expr)
        let sub3 = Sub.compose sub2 sub1
        modifying variables (Map.map (first (apply sub3)))
        pure (sub3, expr)
    EBlock NoExtField block -> do
        (sub1, block) <- second (EBlock NoExtField) <$> infer_block block
        sub2 <- unify (hasInfo block) expected_type (typeOf block)
        let sub3 = Sub.compose sub2 sub1
        modifying variables (Map.map (first (apply sub3)))
        pure (sub3, block)
    Break info maybe_expr -> do
        (sub1, expr) <- infer_break info maybe_expr
        sub2 <- unify info expected_type (typeOf expr)
        let sub3 = Sub.compose sub2 sub1
        modifying variables (Map.map (first (apply sub3)))
        pure (sub3, expr)
    If info condition true false -> do
        (sub1, expr) <- infer_if info condition true false
        sub2 <- unify info expected_type (typeOf expr)
        let sub3 = Sub.compose sub2 sub1
        modifying variables (Map.map (first (apply sub3)))
        pure (sub3, expr)
    While info expr block -> do
        (sub1, expr) <- infer_while info expr block
        sub2 <- unify info expected_type (typeOf expr)
        let sub3 = Sub.compose sub2 sub1
        modifying variables (Map.map (first (apply sub3)))
        pure (sub3, expr)
    Loop info block -> do
        (sub1, expr) <- infer_loop info block
        sub2 <- unify info expected_type (typeOf expr)
        let sub3 = Sub.compose sub2 sub1
        modifying variables (Map.map (first (apply sub3)))
        pure (sub3, expr)
    Lam info args body -> do
        (sub1, expr) <- infer_lam info args body
        sub2 <- unify info expected_type (typeOf expr)
        let sub3 = Sub.compose sub2 sub1
        modifying variables (Map.map (first (apply sub3)))
        pure (sub3, expr)
    Match loc scrutinee match_arms -> do
        (sub1, expr) <- infer_match loc scrutinee match_arms
        sub2 <- unify (hasInfo expr) expected_type (typeOf expr)
        let sub3 = Sub.compose sub2 sub1
        modifying variables (Map.map (first (apply sub3)))
        pure (sub3, expr)


check_exprs ::
    (MonadReader Ctx m, MonadState Env m, MonadValidate [TcError] m) =>
    [(TypeTc, ExprRn)] -> m (Substitution Tc, [ExprTc])
check_exprs = fmap (second reverse) . foldM f (Sub.empty, [])
  where
    f (sub1, exprs) (ty, expr) = do
        (sub2, expr) <- check_expr (apply sub1 ty) expr
        let sub3 = Sub.compose sub2 sub1
        pure (sub3, fmap (apply sub3) (expr : exprs))


-- FIXME(sebsel): Figure out when/how to apply substitution to the environment
infer_expr ::
    (MonadReader Ctx m, MonadState Env m, MonadValidate [TcError] m) =>
    ExprRn -> m (Substitution Tc, ExprTc)
infer_expr current_expr = Ctx.push current_expr $ case current_expr of
    Lit info lit -> infer_lit info lit
    Var (info, namespace, boundedness) name -> infer_var info namespace boundedness name
    Prefix info op expr -> infer_prefix info op expr
    BinOp info left op right -> infer_binop info left op right
    App info lExpr rExprs -> infer_app info lExpr rExprs
    Let (info, mbty) name expr -> infer_let info mbty name expr
    Ass (info, bind, namespace) name op expr -> infer_ass info bind namespace name op expr
    Ret info maybe_expr -> infer_ret info maybe_expr
    EBlock NoExtField block -> second (EBlock NoExtField) <$> infer_block block
    Break info maybe_expr -> infer_break info maybe_expr
    If info condition true false -> infer_if info condition true false
    While info expr block -> infer_while info expr block
    Loop info block -> infer_loop info block
    Lam info args body -> infer_lam info args body
    Match loc scrutinee match_arms -> infer_match loc scrutinee match_arms


infer_exprs ::
    (MonadState Env m, MonadReader Ctx m, MonadValidate [TcError] m) =>
    [ExprRn] -> m (Substitution Tc, [ExprTc])
infer_exprs = fmap (second reverse) . foldM f (Sub.empty, [])
  where
    f (sub1, exprs) expr = do
        (sub2, expr) <- infer_expr expr
        let sub3 = Sub.compose sub2 sub1
        pure (sub3, fmap (apply sub3) (expr : exprs))


infer_lit :: (Monad m) => SourceInfo -> LitRn -> m (Substitution Tc, ExprTc)
infer_lit loc lit = do
    let (ty, lit') = infLit lit
    pure (Sub.empty, Lit (loc, ty) lit')


infer_var ::
    (MonadReader Ctx m, MonadState Env m) =>
    SourceInfo -> Namespace -> Boundedness -> Ident -> m (Substitution Tc, ExprTc)
infer_var loc namespace boundedness name = do
    case boundedness of
        Free; Bound -> do
            (ty, _) <- lookupVar name
            pure (Sub.empty, Var (loc, ty, boundedness) name)
        Function; Imported -> do
            (polytype, info) <- lookupFun namespace name
            let PolyType tyvars ty = polytype
            tbl <- traverse (\ty -> (ty,) <$> fresh_mono) tyvars
            let ty = instantiate_with (Map.fromList tbl) polytype
            pure
                ( Sub.empty
                , Expr
                    $ TypeApp
                        (Var (info, ty, boundedness) name)
                        (fmap (Type . Mono . snd) tbl)
                )
        Constructor -> do
            (polytype, _) <- lookupCon namespace name
            ty <- instantiateTc polytype
            pure (Sub.empty, Var (loc, ty, boundedness) name)
        Builtin -> do
            builtins <- view (defTable . builtIns)
            case Builtins.lookup namespace name builtins of
                Just (ty, info) -> do
                    ty <- instantiateTc ty
                    pure (Sub.empty, Var (info, ty, boundedness) name)
                _ -> __IMPOSSIBLE__


-- pure (Sub.empty, Var (info, ty, boundedness) name)

infer_prefix ::
    (MonadReader Ctx m, MonadState Env m, MonadValidate [TcError] m) =>
    SourceInfo -> PrefixOp -> ExprRn -> m (Substitution Tc, ExprTc)
infer_prefix loc op expr =
    case op of
        Neg -> do
            (sub, expr) <- check_expr (TyLit NoExtField Int) expr
            pure (sub, Prefix (loc, TyLit NoExtField Int) Neg expr)
        Not -> do
            (sub, expr) <- check_expr (TyLit NoExtField Bool) expr
            pure (sub, Prefix (loc, TyLit NoExtField Bool) Not expr)


infer_binop ::
    (MonadReader Ctx m, MonadState Env m, MonadValidate [TcError] m) =>
    SourceInfo -> ExprRn -> BinOp -> ExprRn -> m (Substitution Tc, ExprTc)
infer_binop loc left op right = do
    let expected_type = operatorType op
    (sub1, left) <- check_expr expected_type left
    (sub2, right) <- check_expr expected_type right
    let sub3 = Sub.compose sub2 sub1
    let return_type = operatorReturnType expected_type op
    modifying variables (Map.map (first (apply sub3)))
    pure (sub3, apply sub3 (BinOp (loc, return_type) left op right))


infer_app ::
    (MonadReader Ctx m, MonadState Env m, MonadValidate [TcError] m) =>
    SourceInfo -> ExprRn -> [ExprRn] -> m (Substitution Tc, ExprTc)
infer_app loc func args = do
    (sub1, func) <- infer_expr func
    apply_env sub1
    (argument_types, return_type) <- case typeOf func of
        TyFun _ args ret -> pure (args, ret)
        ty -> applyNonFunction' loc ty
    let number_of_args = length args
    let expected_number_of_args = length argument_types
    case compare (length args) (length argument_types) of
        GT -> partiallyAppliedFunction' loc expected_number_of_args number_of_args
        LT -> tooManyArguments' loc expected_number_of_args number_of_args
        EQ -> do
            (sub2, args) <- check_exprs (zip argument_types args)
            let sub3 = Sub.compose sub2 sub1
            modifying variables (Map.map (first (apply sub3)))
            pure (sub3, apply sub3 $ App (loc, return_type) func args)


infer_let ::
    (MonadReader Ctx m, MonadState Env m, MonadValidate [TcError] m) =>
    SourceInfo -> Maybe TypeRn -> Ident -> ExprRn -> m (Substitution Tc, ExprTc)
infer_let info mbty name expr = case mbty of
    Nothing -> do
        (sub, expr) <- infer_expr expr
        insertVar name (typeOf expr) info
        pure (sub, apply sub $ Let (StmtType unit_type (typeOf expr) info) name expr)
    Just ty -> do
        let ty' = typeOf ty
        (sub, expr) <- check_expr ty' expr
        insertVar name ty' info
        pure (sub, Let (StmtType unit_type ty' info) name expr)


infer_ass ::
    (MonadReader Ctx m, MonadState Env m, MonadValidate [TcError] m) =>
    SourceInfo ->
    Boundedness ->
    Namespace ->
    Ident ->
    AssignOp ->
    ExprRn ->
    m (Substitution Tc, ExprTc)
infer_ass loc boundedness _namespace name op expr =
    case boundedness of
        Function; Imported; Builtin; Constructor -> do
            names <- views Ctx.names (getOriginalName' name)
            assignNonVariable' loc names
        Free; Bound -> do
            (ty, _) <- lookupVar name
            (sub, expr) <- check_expr ty expr
            case op of
                AddAssign; SubAssign; MulAssign; DivAssign; ModAssign -> do
                    let expected_types = [int_type, double_type]
                    unless (ty `elem` expected_types) $ void $ tyExpectedGot loc expected_types ty
                Assign -> pure ()
            pure (sub, Ass (StmtType unit_type ty loc, boundedness) name op expr)


infer_ret ::
    (MonadReader Ctx m, MonadState Env m, MonadValidate [TcError] m) =>
    SourceInfo -> Maybe ExprRn -> m (Substitution Tc, ExprTc)
infer_ret loc maybe_expr = do
    return_type <- view Ctx.returnType
    case maybe_expr of
        Nothing -> do
            sub <- unify loc unit_type return_type
            modifying variables (Map.map (first (apply sub)))
            pure (sub, Ret (loc, return_type) Nothing)
        Just expr -> do
            (sub, expr) <- check_expr return_type expr
            -- NOTE: Use fresh type var here?
            return_type <- view Ctx.returnType
            sub1 <- unify (hasInfo expr) return_type (typeOf expr)
            let sub2 = Sub.compose sub1 sub
            modifying variables (Map.map (first (apply sub2)))
            pure (sub2, apply sub2 $ Ret (loc, return_type) (Just expr))


infer_block ::
    (MonadReader Ctx m, MonadState Env m, MonadValidate [TcError] m) =>
    BlockRn -> m (Substitution Tc, BlockTc)
infer_block (Block info statements tail_expr) = do
    (sub1, stmts) <- infer_stmts statements
    maybe_res <- mapM infer_expr tail_expr
    let break_exprs = find_all_breaks_stmts stmts
    sub1 <- case break_exprs of
        [] -> pure sub1
        (x : xs) ->
            foldM
                (\sub expr -> (`Sub.compose` sub) <$> unify (hasInfo expr) (typeOf x) (apply sub $ typeOf expr))
                sub1
                xs
    case maybe_res of
        Nothing -> do
            modifying variables (Map.map (first (apply sub1)))
            pure (sub1, apply sub1 (Block (info, unit_type) stmts Nothing))
        Just (sub2, expr) -> do
            let sub3 = Sub.compose sub2 sub1
            modifying variables (Map.map (first (apply sub3)))
            pure (sub3, apply sub3 (Block (info, typeOf expr) stmts (Just expr)))


check_block ::
    (MonadReader Ctx m, MonadState Env m, MonadValidate [TcError] m) =>
    TypeTc -> BlockRn -> m (Substitution Tc, BlockTc)
check_block expected_type (Block loc statements tail_expr) = do
    (sub1, statements) <- infer_stmts statements
    maybe_res <- mapM infer_expr tail_expr
    case maybe_res of
        Nothing -> do
            sub2 <- unify loc expected_type unit_type
            let sub3 = Sub.compose sub2 sub1
            modifying variables (Map.map (first (apply sub3)))
            pure (Sub.compose sub2 sub1, apply sub3 (Block (loc, expected_type) statements Nothing))
        Just (sub2, expr) -> do
            sub3 <- unify loc expected_type (typeOf expr)
            let sub4 = sub3 `Sub.compose` sub2 `Sub.compose` sub1
            modifying variables (Map.map (first (apply sub4)))
            pure (sub4, apply sub4 (Block (loc, expected_type) statements (Just expr)))


infer_stmts ::
    (MonadReader Ctx m, MonadState Env m, MonadValidate [TcError] m) =>
    [Stmt Rn] -> m (Substitution Tc, [Stmt Tc])
infer_stmts = fmap (second reverse) . foldM f (Sub.empty, [])
  where
    f (sub1, stmts) stmt = do
        (sub2, stmt) <- infer_stmt stmt
        pure (Sub.compose sub2 sub1, stmt : stmts)


infer_stmt ::
    (MonadReader Ctx m, MonadState Env m, MonadValidate [TcError] m) =>
    Stmt Rn -> m (Substitution Tc, Stmt Tc)
infer_stmt (SExpr NoExtField expr) = second (SExpr NoExtField) <$> infer_expr expr


infer_break ::
    (MonadReader Ctx m, MonadState Env m, MonadValidate [TcError] m) =>
    SourceInfo -> Maybe (Expr Rn) -> m (Substitution Tc, ExprTc)
infer_break loc maybe_expr = case maybe_expr of
    Nothing -> pure (Sub.empty, Break (loc, unit_type) Nothing)
    Just expr -> do
        (sub, expr) <- infer_expr expr
        pure (sub, Break (loc, typeOf expr) (Just expr))


infer_if ::
    (MonadReader Ctx m, MonadState Env m, MonadValidate [TcError] m) =>
    SourceInfo -> Expr Rn -> Block Rn -> Maybe (Block Rn) -> m (Substitution Tc, ExprTc)
infer_if loc scrutinee true_block false_block = do
    (sub1, scrutinee) <- check_expr bool_type scrutinee
    (sub2, true_block) <- infer_block true_block
    maybe_res <- mapM (check_block (typeOf true_block)) false_block
    let sub3 = Sub.compose sub2 sub1
    case maybe_res of
        Nothing -> do
            modifying variables (Map.map (first (apply sub3)))
            pure (sub3, If (loc, typeOf true_block) scrutinee true_block Nothing)
        Just (sub4, false_block) -> do
            sub5 <- unify loc (typeOf true_block) (typeOf false_block)
            let sub6 = sub5 `Sub.compose` sub4 `Sub.compose` sub3
            modifying variables (Map.map (first (apply sub6)))
            pure
                ( sub6
                , apply
                    sub6
                    (If (loc, typeOf true_block) scrutinee true_block (Just false_block))
                )


infer_while ::
    (MonadReader Ctx m, MonadState Env m, MonadValidate [TcError] m) =>
    SourceInfo -> Expr Rn -> Block Rn -> m (Substitution Tc, ExprTc)
infer_while loc expr block = do
    (sub1, expr) <- check_expr bool_type expr
    (sub2, block) <- infer_block block
    let sub3 = Sub.compose sub2 sub1
    sub4 <- unify loc unit_type (typeOf block)
    let sub5 = Sub.compose sub4 sub3
    modifying variables (Map.map (first (apply sub5)))
    pure (sub5, apply sub5 (While (loc, typeOf block) expr block))


infer_loop ::
    (MonadReader Ctx m, MonadState Env m, MonadValidate [TcError] m) =>
    SourceInfo -> Block Rn -> m (Substitution Tc, ExprTc)
infer_loop loc block = do
    (sub, block) <- infer_block block
    let break_exprs = concatMap find_all_breaks (get_exprs block)
    break_type <- case break_exprs of
        [] -> pure Any
        (x : xs) -> do
            sub <-
                foldM
                    ( \sub expr ->
                        (`Sub.compose` sub)
                            <$> unify (hasInfo expr) (typeOf x) (apply sub $ typeOf expr)
                    )
                    sub
                    xs
            pure (apply sub $ typeOf x)
    sub1 <- unify loc unit_type (typeOf block)
    let sub2 = Sub.compose sub1 sub
    modifying variables (Map.map (first (apply sub2)))
    pure (sub2, apply sub2 $ Loop (loc, break_type) block)


infer_lam ::
    (MonadReader Ctx m, MonadState Env m, MonadValidate [TcError] m) =>
    SourceInfo -> [LamArg Rn] -> Expr Rn -> m (Substitution Tc, ExprTc)
infer_lam loc args expr = do
    args <- mapM infer_lamarg args
    (sub, expr) <- infer_expr expr
    modifying variables (Map.map (first (apply sub)))
    pure (sub, apply sub $ Lam (loc, TyFun NoExtField (fmap typeOf args) (typeOf expr)) args expr)


infer_lamarg ::
    (MonadReader Ctx m, MonadState Env m, MonadValidate [TcError] m) =>
    LamArgRn -> m LamArgTc
infer_lamarg (LamArg (info, maybe_type, _namespace) name) = do
    ty <- maybe fresh_type (pure . typeOf) maybe_type
    insertVar name ty info
    pure (LamArg ty name)


infer_match ::
    (MonadReader Ctx m, MonadState Env m, MonadValidate [TcError] m) =>
    SourceInfo -> Expr Rn -> [MatchArm Rn] -> m (Substitution Tc, ExprTc)
infer_match loc scrutinee arms = do
    (sub, scrutinee) <- infer_expr scrutinee
    let scrutinee_type = typeOf scrutinee
    case arms of
        [] -> do
            modifying variables (Map.map (first (apply sub)))
            pure (sub, Match (loc, Any) scrutinee [])
        (arm1 : arms) -> do
            (sub1, arm) <- infer_arm scrutinee_type arm1
            let arm_type = typeOf arm
            let f (sub1, arms) arm = do
                    (sub2, arm) <- check_arm scrutinee_type arm_type arm
                    pure (Sub.compose sub2 sub1, arm : arms)
            (sub2, arms) <- foldM f (Sub.compose sub1 sub, []) arms
            modifying variables (Map.map (first (apply sub2)))
            pure (sub2, Match (loc, typeOf arm) scrutinee (arm : arms))


check_arm ::
    (MonadReader Ctx m, MonadState Env m, MonadValidate [TcError] m) =>
    TypeTc -> TypeTc -> MatchArmRn -> m (Substitution Tc, MatchArmTc)
check_arm pattern_type arm_type (MatchArm loc pat expr) = do
    (sub1, pat) <- check_pattern pattern_type pat
    (sub2, body) <- check_expr arm_type expr
    let sub3 = Sub.compose sub2 sub1
    modifying variables (Map.map (first (apply sub3)))
    pure (sub3, apply sub3 (MatchArm loc pat body))


infer_arm ::
    (MonadReader Ctx m, MonadState Env m, MonadValidate [TcError] m) =>
    TypeTc -> MatchArmRn -> m (Substitution Tc, MatchArmTc)
infer_arm pattern_type (MatchArm loc pat expr) = do
    (sub1, pat) <- check_pattern pattern_type pat
    (sub2, body) <- infer_expr expr
    let sub3 = Sub.compose sub2 sub1
    modifying variables (Map.map (first (apply sub3)))
    pure (sub3, apply sub3 (MatchArm loc pat body))


check_pattern ::
    (MonadReader Ctx m, MonadState Env m, MonadValidate [TcError] m) =>
    TypeTc -> PatternRn -> m (Substitution Tc, PatternTc)
check_pattern pattype currentPattern = case currentPattern of
    PVar loc varName -> do
        insertVar varName pattype loc
        pure (Sub.empty, PVar (loc, pattype) varName)
    PEnumCon (_, namespace) conName -> do
        (ty, loc) <- lookupCon namespace conName
        ty <- instantiateTc ty
        sub <- unify loc pattype ty
        modifying variables (Map.map (first (apply sub)))
        pure (sub, apply sub $ PEnumCon (loc, ty) conName)
    PFunCon (_, namespace) conName pats -> do
        (ty, loc) <- lookupCon namespace conName
        ty <- instantiateTc ty
        case ty of
            TyFun _ argtys rettype
                | length argtys == length pats -> do
                    sub1 <- unify loc pattype rettype
                    let f (sub1, pats) (expected_type, pat) = do
                            (sub2, pat) <- check_pattern expected_type pat
                            pure (Sub.compose sub2 sub1, pat : pats)
                    (sub2, pats) <- foldM f (sub1, []) $ zip argtys pats
                    let sub3 = Sub.compose sub2 sub1
                    modifying variables (Map.map (first (apply sub3)))
                    pure (sub3, apply sub3 $ PFunCon (loc, pattype) conName pats)
                | otherwise -> expectedPatNArgs' loc currentPattern (length argtys) (length pats)
            other -> tyExpectedGot' loc [pattype] other


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


unit_type :: TypeTc
unit_type = TyLit NoExtField Unit


int_type :: TypeTc
int_type = TyLit NoExtField Int


double_type :: TypeTc
double_type = TyLit NoExtField Double


bool_type :: TypeTc
bool_type = TyLit NoExtField Bool


insertVar :: (MonadState Env m) => Ident -> Type Tc -> SourceInfo -> m ()
insertVar name ty info = modifying variables (Map.insert name (ty, info))


lookupVar :: (MonadState Env m) => Ident -> m (Type Tc, SourceInfo)
lookupVar name =
    uses
        variables
        ( fromMaybe __IMPOSSIBLE__
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
        ( fromMaybe __IMPOSSIBLE__
            . (Map.lookup name <=< Map.lookup namespace)
        )


apply_env :: (MonadState Env m) => Substitution Tc -> m ()
apply_env sub = modifying variables (Map.map (first (apply sub)))


instantiateTc :: (MonadState Env m) => PolyType Tc -> m TypeTc
instantiateTc ty = do
    fr <- use fresh
    let (ty', fr') = instantiate fr ty
    assign fresh fr'
    pure ty'


fresh_type :: (MonadState Env m) => m TypeTc
fresh_type = Type . Mono <$> fresh_mono


fresh_mono :: (MonadState Env m) => m MonoType
fresh_mono = do
    fr <- use fresh
    fresh += 1
    pure (MonoType fr)


get_exprs :: Block Tc -> [Expr Tc]
get_exprs (Block _ exprs tail) = fmap (\(SExpr NoExtField expr) -> expr) exprs <> maybeToList tail


find_all_breaks_stmts :: [StmtTc] -> [ExprTc]
find_all_breaks_stmts = concatMap (find_all_breaks . (\(SExpr NoExtField expr) -> expr))


-- \| Find all breaks in a block. Do not traverse further on expressions where breaks are allowed
find_all_breaks :: ExprTc -> [ExprTc]
find_all_breaks e = case e of
    Lit {} -> []
    Var {} -> []
    Break {} -> [e]
    If _ cond l r ->
        find_all_breaks cond
            <> concatMap find_all_breaks (get_exprs l)
            <> maybe [] (concatMap find_all_breaks . get_exprs) r
    BinOp _ l _ r -> find_all_breaks l <> find_all_breaks r
    Prefix _ _ e -> find_all_breaks e
    App _ l rs -> find_all_breaks l <> concatMap find_all_breaks rs
    Let _ _ expr -> find_all_breaks expr
    Ass _ _ _ expr -> find_all_breaks expr
    Ret _ expr -> maybe [] find_all_breaks expr
    EBlock _ block -> concatMap find_all_breaks (get_exprs block)
    While {} -> []
    Loop {} -> []
    Lam {} -> []
    Match _ scrutinee arms -> find_all_breaks scrutinee <> concatMap breakArm arms
  where
    breakArm :: MatchArm Tc -> [ExprTc]
    breakArm (MatchArm _ _ body) = find_all_breaks body
