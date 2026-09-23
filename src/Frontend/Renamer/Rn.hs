{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use lambda-case" #-}

module Frontend.Renamer.Rn (rename) where

import Control.Lens (locally, modifying, view)
import Control.Monad.Validate (MonadValidate (dispute, refute))
import Data.Map qualified as Map
import Data.Set qualified as Set
import Frontend.Builtin (builtins)
import Frontend.Builtin qualified as Builtins
import Frontend.Error
import Frontend.Parser.Types
import Frontend.Renamer.Monad
import Frontend.Renamer.Types
import Frontend.Types
import Frontend.Utils (isUnique)
import Names (Ident (..), Names, Namespace (Namespace), getText, mkNames)
import Relude hiding (intercalate)
import Utils (listify')

rename :: Set Namespace -> Map Ident Namespace -> ProgramPar -> Either [RnError] (ProgramRn, Names)
rename namespaces symbolMap prg@(Program namespace defs) =
    runGen (emptyEnv symbolMap) (createCtx namespace builtins allVars namespaces types)
        $ rnProgram prg
  where
    types =
        Set.fromList
            (mapMaybe (\case DefAdt (Adt _loc name _cons) -> Just name; _ -> Nothing) defs)
    allVars :: Map Namespace (Set Ident)
    allVars =
        foldr (\(k, v) acc -> Map.insertWith Set.union k (Set.singleton v) acc) mempty
            $ listify' f prg
      where
        f :: ExprPar -> Maybe (Namespace, Ident)
        f (Var (_, ns) name) = Just (fromMaybe namespace ns, name)
        f _ = Nothing

rnProgram :: ProgramPar -> Gen (ProgramRn, Names)
rnProgram program@(Program a defs) = do
    let functions = getFunctionNames program
    let adts = getAdtNames program
    uniqueDefs adts
    uniqueDefs functions
    let toplevelSet = Set.fromList $ fmap snd functions
    defs <- locally localDefinitions (Set.union toplevelSet) (mapM rnDef (sortDefs defs))
    names <- names
    pure (Program a defs, mkNames names)

sortDefs :: [Def a] -> [Def a]
sortDefs = sortBy f
  where
    f :: Def a -> Def a -> Ordering
    f (DefImport _) (DefImport _) = EQ
    f (DefImport _) _ = LT
    f (DefAdt _) (DefImport _) = GT
    f (DefAdt _) (DefAdt _) = EQ
    f (DefAdt _) _ = EQ
    f (DefX _) (DefFn _) = LT
    f (DefX _) (DefX _) = EQ
    f (DefX _) _ = GT
    f (DefFn _) (DefFn _) = EQ
    f (DefFn _) _ = GT

uniqueDefs :: (MonadValidate [RnError] m) => [(SourceInfo, Ident)] -> m ()
uniqueDefs = go (Builtins.names builtins)
  where
    go :: (MonadValidate [RnError] m) => Set Ident -> [(SourceInfo, Ident)] -> m ()
    go _ [] = pure ()
    go seen ((info, name) : xs) =
        if Set.member name seen
            then duplicateToplevels info name
            else go (Set.insert name seen) xs

rnFunction :: FnPar -> Gen FnRn
rnFunction (Fn pos name tyParams arguments returnType block) = do
    resetArgs
    case isUnique tyParams of
        Nothing -> pure ()
        Just (loc, duped) -> dispute [ConflictingTypeParameter loc duped]
    arguments <- rnArgs tyParams arguments
    returnType <- renameType tyParams returnType
    statements <- rnBlock tyParams block
    return $ Fn pos name tyParams arguments returnType statements

rnDef :: DefPar -> Gen DefRn
rnDef (DefFn fn) = DefFn <$> rnFunction fn
rnDef (DefAdt adt) = DefAdt <$> rnAdt adt
rnDef (DefImport imp) = DefImport <$> rnImport imp

{-|
Transforms all non-explicit imports to explicit imports.

`import foo.bar (baz)` is transformed to `import foo.bar (baz)` and `baz`
    is transformed to `foo.bar.baz` at the usage sites.

Transforms `import foo.bar as baz` to `import foo.bar (f)`
    if `baz.f` is used somewhere in the program,
    `baz.f` is in turn transformed to `foo.bar.f` the usage sites
Transforms `import foo.bar` to `import foo.bar (f)`
    if `foo.bar.f` is used somewhere in the program.
-}
rnImport :: ImportPar -> Gen ImportRn
rnImport (ImportExplicit loc namespace symbols) = do
    exist <- doesNamespaceExist namespace
    unless exist $ unboundImport loc namespace
    modifying importedDefinitions (\acc -> foldr (`Map.insert` namespace) acc symbols)
    pure (ImportExplicit loc namespace symbols)
rnImport (XImport extraimport) = rnExtraImport extraimport
  where
    rnExtraImport :: ExtraImports SourceInfo -> Gen ImportRn
    rnExtraImport (ImportAs namespace name loc) = do
        exist <- doesNamespaceExist namespace
        unless exist $ unboundImport loc namespace
        insertImportName name namespace
        defs <- view allVars
        let symbols =
                sort
                    $ Map.foldrWithKey
                        ( \k v acc ->
                            if Namespace (return (getText name)) == k
                                then Set.toList v <> acc
                                else acc
                        )
                        []
                        defs
        pure (ImportExplicit loc namespace symbols)
    rnExtraImport (ImportQualified namespace loc) = do
        exist <- doesNamespaceExist namespace
        unless exist $ unboundImport loc namespace
        defs <- view allVars
        let symbols =
                sort
                    $ Map.foldrWithKey
                        ( \k v acc ->
                            if namespace == k
                                then Set.toList v <> acc
                                else acc
                        )
                        []
                        defs
        pure (ImportExplicit loc namespace symbols)

rnAdt :: AdtPar -> Gen AdtRn
rnAdt (Adt loc name constructors) = Adt loc name <$> mapM rnConstructor constructors

rnConstructor :: ConstructorPar -> Gen ConstructorRn
rnConstructor = \case
    EnumCons loc name -> checkAndinsertConstrutor loc name >> pure (EnumCons loc name)
    FunCons loc name types ->
        checkAndinsertConstrutor loc name
            >> FunCons loc name <$> mapM (renameType emptyTyParamList) types

rnBlock :: TyParamList -> BlockPar -> Gen BlockRn
rnBlock tyParams (Block a stmts expr) =
    uncurry (Block a)
        <$> newContext ((,) <$> mapM (rnStatement tyParams) stmts <*> mapM (rnExpr tyParams) expr)

rnStatement :: TyParamList -> StmtPar -> Gen StmtRn
rnStatement tyParams = \case
    SExpr a b -> do
        b <- rnExpr tyParams b
        pure $ SExpr a b

rnExpr :: TyParamList -> ExprPar -> Gen ExprRn
rnExpr tyParams = goRnExpr
  where
    goRnExpr :: ExprPar -> Gen ExprRn
    goRnExpr = \case
        Lit info lit -> Lit info <$> rnLit lit
        Var (info, ns) variable -> do
            namespace <- view namespace
            (bind, (namespace, name)) <-
                maybe
                    ((Free, (Namespace ("$unbound$" :| []), Ident "$unbound$")) <$ unboundVariable info variable)
                    pure
                    =<< maybe (fmap (\(a, b, c) -> (a, (b, c))) <$> boundImported namespace variable) (pure . Just)
                    =<< maybe (fmap (Constructor,) <$> boundCons variable) (pure . Just)
                    =<< maybe
                        ( case ns of
                            Just namespace -> fmap (Builtin,) <$> lookupBuiltin namespace variable
                            Nothing -> pure Nothing
                        )
                        (pure . Just)
                    =<< maybe (fmap (\x -> (Toplevel, (namespace, x))) <$> boundFun variable) (pure . Just)
                    =<< ( maybe
                            (fmap (Free,) <$> boundArg variable)
                            ((pure . Just) . (\(a, b, c) -> (a, (b, c))))
                            =<< boundVar variable
                        )
            pure $ Var (info, namespace, bind) name
        Prefix info op expr -> Prefix info op <$> goRnExpr expr
        BinOp info l op r -> do
            l <- goRnExpr l
            r <- goRnExpr r
            pure $ BinOp info l op r
        App info l args -> do
            l <- goRnExpr l
            args <- mapM goRnExpr args
            pure $ App info l args
        Let (info, ty) name expr -> do
            expr <- goRnExpr expr
            name' <- insertVar name
            ty <- mapM (renameType tyParams) ty
            pure $ Let (info, ty) name' expr
        Ass info variable op expr -> do
            namespace <- view namespace
            (bind, (namespace, name)) <-
                maybe ((Free, (namespace, Ident "unbound")) <$ unboundVariable info variable) pure
                    =<< ( maybe
                            (fmap (Free,) <$> boundArg variable)
                            ((pure . Just) . (\(a, b, c) -> (a, (b, c))))
                            =<< boundVar variable
                        )
            expr <- goRnExpr expr
            pure (Ass (info, bind, namespace) name op expr)
        Ret a b -> do
            b' <- mapM goRnExpr b
            pure $ Ret a b'
        EBlock info block -> EBlock info <$> rnBlock tyParams block
        Break a expr -> do
            b' <- mapM goRnExpr expr
            pure $ Break a b'
        If a b true false -> do
            b <- goRnExpr b
            true <- newContext $ rnBlock tyParams true
            false <- newContext $ mapM (rnBlock tyParams) false
            pure $ If a b true false
        While a b block -> do
            b <- goRnExpr b
            stmts <- newContext $ rnBlock tyParams block
            pure $ While a b stmts
        Loop info block -> Loop info <$> rnBlock tyParams block
        Lam info args body -> do
            args <- rnLamArgs tyParams args
            body <- newContext $ goRnExpr body
            pure $ Lam info args body
        Match info scrutinee arms -> do
            scrutinee <- goRnExpr scrutinee
            arms <- mapM (rnMatchArm tyParams) arms
            pure $ Match info scrutinee arms

rnMatchArm :: TyParamList -> MatchArmPar -> Gen MatchArmRn
rnMatchArm tyParams (MatchArm loc pat body) = newContext $ do
    pat <- rnPattern pat
    body <- rnExpr tyParams body
    pure $ MatchArm loc pat body

rnPattern :: PatternPar -> Gen PatternRn
rnPattern = fmap snd . go mempty
  where
    go :: [Ident] -> PatternPar -> Gen ([Ident], PatternRn)
    go seen = \case
        PVar loc varName -> do
            when (varName `elem` seen) (conflictingDefinitionArgument loc varName)
            name <- insertVar varName
            pure (varName : seen, PVar loc name)
        PEnumCon loc conName -> do
            ns <- view namespace
            pure ([], PEnumCon (loc, ns) conName)
        PFunCon loc conName pats -> do
            ns <- view namespace
            (seen, pats) <- go' seen pats
            pure (seen, PFunCon (loc, ns) conName pats)
          where
            go' :: [Ident] -> [PatternPar] -> Gen ([Ident], [PatternRn])
            go' seen [] = pure (seen, [])
            go' seen (x : xs) = do
                (seen', pat) <- go seen x
                (seen'', pats) <- go' (seen <> seen') xs
                pure (seen <> seen' <> seen'', pat : pats)

rnLamArgs ::
    (MonadState Env m, MonadValidate [RnError] m, MonadReader Ctx m) =>
    TyParamList -> [LamArgPar] -> m [LamArgRn]
rnLamArgs tyParams = fmap (reverse . snd) . foldlM f mempty
  where
    f ::
        (MonadState Env m, MonadValidate [RnError] m, MonadReader Ctx m) =>
        ([Ident], [LamArgRn]) ->
        LamArgPar ->
        m ([Ident], [LamArgRn])
    f (seen, acc) (LamArg (info, ty) name) = do
        namespace <- view namespace
        let seen' = name : seen
        when (name `elem` seen) (conflictingDefinitionArgument info name)
        (namespace, name) <- insertArg namespace name
        ty <- mapM (renameType tyParams) ty
        pure (seen', LamArg (info, ty, namespace) name : acc)

rnLit :: LitPar -> Gen LitRn
rnLit = \case
    IntLit info lit -> pure $ IntLit info lit
    DoubleLit info lit -> pure $ DoubleLit info lit
    StringLit info lit -> pure $ StringLit info lit
    CharLit info lit -> pure $ CharLit info lit
    BoolLit info lit -> pure $ BoolLit info lit
    UnitLit info -> pure $ UnitLit info

getAdtNames :: ProgramPar -> [(SourceInfo, Ident)]
getAdtNames = listify' adtName
  where
    adtName :: AdtPar -> Maybe (SourceInfo, Ident)
    adtName (Adt info name _) = Just (info, name)

getFunctionNames :: ProgramPar -> [(SourceInfo, Ident)]
getFunctionNames = listify' fnName
  where
    fnName :: FnPar -> Maybe (SourceInfo, Ident)
    fnName (Fn info name _ _ _ _) = Just (info, name)

rnArgs ::
    (MonadState Env m, MonadValidate [RnError] m, MonadReader Ctx m) =>
    TyParamList -> [ArgPar] -> m [ArgRn]
rnArgs tyParams = fmap (reverse . snd) . foldlM f mempty
  where
    f ::
        (MonadState Env m, MonadValidate [RnError] m, MonadReader Ctx m) =>
        ([Ident], [ArgRn]) ->
        ArgPar ->
        m ([Ident], [ArgRn])
    f (seen, acc) (Arg info name ty) = do
        namespace <- view namespace
        let seen' = name : seen
        when (name `elem` seen) (conflictingDefinitionArgument info name)
        (namespace, name) <- insertArg namespace name
        ty <- renameType tyParams ty
        pure (seen', Arg (info, namespace) name ty : acc)

renameType :: (MonadReader Ctx m, MonadValidate [RnError] m) => TyParamList -> TypePar -> m TypeRn
renameType typeParams ty = do
    userDefinedTypes <- view userDefinedTypes
    case ty of
        TyCon loc name -> pure (TyCon NoExtField name)
        TypeVar loc tyvar -> pure (TypeVar NoExtField tyvar)
        TyLit loc lit -> pure (TyLit NoExtField lit)
        TyFun loc args ret -> TyFun NoExtField <$> mapM (renameType typeParams) args <*> renameType typeParams ret
        Type unresolved@(UnresolvedType loc _)
            | isTypeVar unresolved typeParams -> pure (TypeVar NoExtField (tyVarOf unresolved))
            | Set.member (nameOf unresolved) userDefinedTypes -> pure (TyCon NoExtField (nameOf unresolved))
            | otherwise -> refute [UnboundType loc (nameOf unresolved)]
