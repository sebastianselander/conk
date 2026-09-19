{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Frontend.Renamer.Rn (rename) where

import Control.Lens (locally, modifying, view)
import Control.Monad.Validate (MonadValidate)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Frontend.Builtin (builtInNames, builtIns)
import Frontend.Error
import Frontend.Parser.Types
import Frontend.Renamer.Monad
import Frontend.Renamer.Types
import Frontend.Types
import Names (Ident (..), Names, Namespace (Namespace), getText, mkNames)
import Relude hiding (intercalate)
import Utils (listify')

rename :: Map Ident Namespace -> ProgramPar -> Either [RnError] (ProgramRn, Names)
rename symbolMap prg@(Program namespace _) =
    runGen (emptyEnv symbolMap) (emptyCtx namespace (resolve (builtIns @Par)) allVars) $ rnProgram prg
  where
    resolve :: Map Namespace (Map Ident b) -> Map Namespace (Map Ident Ident)
    resolve = Map.map (Map.mapWithKey const)
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
uniqueDefs = go builtInNames
  where
    go :: (MonadValidate [RnError] m) => Set Ident -> [(SourceInfo, Ident)] -> m ()
    go _ [] = pure ()
    go seen ((info, name) : xs) =
        if Set.member name seen
            then duplicateToplevels info name
            else go (Set.insert name seen) xs

rnFunction :: FnPar -> Gen FnRn
rnFunction (Fn pos name arguments returnType block) = do
    resetArgs
    arguments <- rnArgs arguments
    returnType <- rnType returnType
    statements <- rnBlock block
    return $ Fn pos name arguments returnType statements

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
    modifying importedDefinitions (\acc -> foldr (`Map.insert` namespace) acc symbols)
    pure (ImportExplicit loc namespace symbols)
rnImport (XImport extraimport) = rnExtraImport extraimport
  where
    rnExtraImport :: ExtraImports SourceInfo -> Gen ImportRn
    rnExtraImport (ImportAs namespace name loc) = do
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
            >> FunCons loc name
            <$> mapM rnType types

rnBlock :: BlockPar -> Gen BlockRn
rnBlock (Block a stmts expr) =
    uncurry (Block a) <$> newContext ((,) <$> mapM rnStatement stmts <*> mapM rnExpr expr)

rnStatement :: StmtPar -> Gen StmtRn
rnStatement = \case
    SExpr a b -> do
        b <- rnExpr b
        pure $ SExpr a b

rnExpr :: ExprPar -> Gen ExprRn
rnExpr = \case
    Lit info lit -> Lit info <$> rnLit lit
    Var (info, ns) variable -> do
        namespace <- view namespace
        (bind, (namespace, name)) <-
            maybe
                ((Free, (Namespace ("$unbound$" :| []), Ident "$unbound$")) <$ unboundVariable info variable)
                pure
                =<< maybe (fmap (Constructor,) <$> boundCons variable) (pure . Just)
                =<< maybe
                    ( case ns of
                        Just namespace -> fmap (Builtin,) <$> isBuiltin namespace variable
                        Nothing -> pure Nothing
                    )
                    (pure . Just)
                =<< maybe (fmap (\x -> (Toplevel, (namespace, x))) <$> boundFun variable) (pure . Just)
                =<< maybe (fmap (\(a, b, c) -> (a, (b, c))) <$> boundImported variable) (pure . Just)
                =<< ( maybe
                        (fmap (Free,) <$> boundArg variable)
                        ((pure . Just) . (\(a, b, c) -> (a, (b, c))))
                        =<< boundVar variable
                    )
        pure $ Var (info, namespace, bind) name
    Prefix info op expr -> Prefix info op <$> rnExpr expr
    BinOp info l op r -> do
        l <- rnExpr l
        r <- rnExpr r
        pure $ BinOp info l op r
    App info l args -> do
        l <- rnExpr l
        args <- mapM rnExpr args
        pure $ App info l args
    Let (info, ty) name expr -> do
        expr <- rnExpr expr
        name' <- insertVar name
        ty <- mapM rnType ty
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
        expr <- rnExpr expr
        pure (Ass (info, bind, namespace) name op expr)
    Ret a b -> do
        b' <- mapM rnExpr b
        pure $ Ret a b'
    EBlock info block -> EBlock info <$> rnBlock block
    Break a expr -> do
        b' <- mapM rnExpr expr
        pure $ Break a b'
    If a b true false -> do
        b <- rnExpr b
        true <- newContext $ rnBlock true
        false <- newContext $ mapM rnBlock false
        pure $ If a b true false
    While a b block -> do
        b <- rnExpr b
        stmts <- newContext $ rnBlock block
        pure $ While a b stmts
    Loop info block -> Loop info <$> rnBlock block
    Lam info args body -> do
        args <- rnLamArgs args
        body <- newContext $ rnExpr body
        pure $ Lam info args body
    Match info scrutinee arms -> do
        scrutinee <- rnExpr scrutinee
        arms <- mapM rnMatchArm arms
        pure $ Match info scrutinee arms

rnMatchArm :: MatchArmPar -> Gen MatchArmRn
rnMatchArm (MatchArm loc pat body) = newContext $ do
    pat <- rnPattern pat
    body <- rnExpr body
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
    (MonadState Env m, MonadValidate [RnError] m, MonadReader Ctx m) => [LamArgPar] -> m [LamArgRn]
rnLamArgs = fmap (reverse . snd) . foldlM f mempty
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
        ty <- mapM rnType ty
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
    fnName (Fn info name _ _ _) = Just (info, name)

rnArgs :: (MonadState Env m, MonadValidate [RnError] m, MonadReader Ctx m) => [ArgPar] -> m [ArgRn]
rnArgs = fmap (reverse . snd) . foldlM f mempty
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
        ty <- rnType ty
        pure (seen', Arg (info, namespace) name ty : acc)

rnType :: (Monad m) => TypePar -> m TypeRn
rnType = pure . coerceType
