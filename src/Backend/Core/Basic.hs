{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Backend.Core.Basic (basicCore) where

import Control.Lens (makeLenses)
import Control.Lens.Getter (use, uses, view)
import Control.Lens.Setter (assign, modifying, (+=))
import Control.Monad.Extra (concatMapM)
import Data.DList hiding (concat)
import Data.Maybe (fromJust)
import Data.Tuple.Extra (uncurry3)
import Relude hiding (Type, fromList, toList)

import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as Text

import Backend.Core.Types
import Backend.Llvm.Prelude (globalUnit)
import Backend.Types
import Frontend.Typechecker.Types (FnType (..), MetaTy (Mono), stmtType, varType)
import Frontend.Types
    ( Import (ImportExplicit),
      NoExtField (NoExtField),
      SourceInfo,
    )
import Impossible (__IMPOSSIBLE__)
import Names (Ident (..), Names, Namespace, existName, insertName)
import Utils (mapWithIndexM)

import Frontend.Typechecker.Types qualified as Tc
import Frontend.Types qualified as Bound (Boundedness (..))
import Frontend.Types qualified as Tc
import Origin qualified as Origin


data Env = Env
    { _expressions :: DList TyExpr
    , _names :: Names
    , _nameCounter :: Int
    , _lifted :: DList Def
    , _staticStrings :: [(Ident, Type, Text)]
    , _constructorIndex :: Map Ident Int
    }


$(makeLenses ''Env)


newtype DsM a = DsM {runDsm :: StateT Env (Reader (TyExpr -> DsM ())) a}
    deriving (Applicative, Functor, Monad, MonadReader (TyExpr -> DsM ()), MonadState Env)


run :: Map Ident Int -> (TyExpr -> DsM ()) -> Names -> Int -> DsM a -> (a, Env)
run cons f names n = flip runReader f . flip runStateT (Env mempty names n mempty mempty cons) . runDsm


emit :: TyExpr -> DsM ()
emit expr = modifying expressions (`snoc` expr)


emits :: [TyExpr] -> DsM ()
emits = mapM_ emit


named :: DsM TyExpr -> DsM TyExpr
named tyExpr = do
    (Typed ty e) <- tyExpr
    name <- fresh "named"
    emit $ Typed Unit $ Let name ty (Just (Typed ty e))
    pure (Typed ty (Var Bound name))


unitVarible :: DsM TyExpr
unitVarible = named (pure unit)


unitGlobalName :: Ident
unitGlobalName = Ident globalUnit


unitGlobalVariable :: DsM TyExpr
unitGlobalVariable = pure $ Typed Unit (Var GlobalConstant unitGlobalName)


unnamed :: DsM TyExpr -> DsM ()
unnamed thing = do
    expr <- thing
    emit expr
    pure ()


declare :: Type -> DsM Ident
declare (I 1) = do
    name <- fresh "declare"
    emit $ Typed Unit $ Let name Unit (Just unit)
    pure name
declare ty = do
    name <- fresh "declare"
    emit $ Typed Unit $ Let name ty Nothing
    pure name


fresh :: Text -> DsM Ident
fresh prefix = go 0
  where
    go :: Int -> DsM Ident
    go n = do
        let freshName = Ident $ prefix <> "_" <> show n
        nameMap <- use names
        if existName freshName nameMap
            then go (n + 1)
            else do
                modifying names (insertName freshName)
                pure freshName


basicCore :: Names -> Tc.ProgramTc -> Program
basicCore names = fst . run mempty (const $ pure ()) names 0 . dsProgram


dsProgram :: Tc.ProgramTc -> DsM Program
dsProgram (Tc.Program Tc.NoExtField defs) = do
    defs <- concatMapM dsDef defs
    lifteds <- use lifted
    strings <- use staticStrings
    pure $ Program $ fmap (uncurry3 StaticString) strings <> toList lifteds <> defs


isMain :: Tc.FnTc -> Bool
isMain (Tc.Fn NoExtField (Ident "main") _tyParams _ _ _) = True
isMain _ = False


dsFunction :: Tc.FnTc -> DsM Def
dsFunction def@(Tc.Fn NoExtField name _tyParams args returnType (Tc.Block (_info, _) stmts tail)) = do
    assign nameCounter 0 -- Start the name counter from 0 for each local scope
    args <- mapM dsArg args
    returnType <- dsType returnType
    case tail of
        Nothing -> mapM_ dsStmt stmts
        Just tail -> do
            mapM_ dsStmt stmts
            tail <- dsExpr tail
            if isMain def
                then void (unnamed (pure tail))
                else void $ unnamed $ pure $ Typed returnType (Return tail)
    emits <- use expressions
    assign expressions mempty
    if isMain def
        then pure $ Main (toList emits)
        else case returnType of
            (I 1) -> pure $ Fn Origin.Function name args returnType (toList $ emits `snoc` Typed (I 1) (Return unit))
            _ -> pure $ Fn Origin.Function name args returnType (toList emits)


dsDef :: Tc.DefTc -> DsM [Def]
dsDef (Tc.DefFn fn) = pure <$> dsFunction fn
dsDef (Tc.DefAdt adt) = dsAdt adt
dsDef (Tc.DefImport (ImportExplicit tys namespace names)) = zipWithM (declare namespace) tys names
  where
    declare :: (Monad m) => Namespace -> FnType -> Ident -> m Def
    declare namespace fnType name =
        Decl namespace <$> dsType fnType.retType <*> pure name <*> mapM dsType fnType.argTypes


dsAdt :: Tc.AdtTc -> DsM [Def]
dsAdt (Tc.Adt _loc name constructors) = do
    alloc <- constructorAllocSize constructors
    funs <- mapWithIndexM mkConstructorFunction constructors
    case alloc of
        0 -> pure $ TypeSyn name (StructType [Int64]) : funs
        n -> pure $ TypeSyn name (StructType [Int64, ArrayType (n `div` 8) opaquePtr]) : funs


mkConstructorFunction :: Int -> Tc.ConstructorTc -> DsM Def
mkConstructorFunction n = \case
    Tc.EnumCons (_loc, ty) name -> do
        modifying constructorIndex (Map.insert name n)
        Con n name <$> dsType ty <*> pure Nothing
    Tc.FunCons (_loc, ty) name tys -> do
        modifying constructorIndex (Map.insert name n)
        Con n name <$> dsType ty <*> Just <$> mapM dsType tys


constructorAllocSize :: (Monad m) => [Tc.ConstructorTc] -> m Int
constructorAllocSize [] = pure 0
constructorAllocSize (Tc.EnumCons (_loc, _) _ : cons) = fmap (max 0) (constructorAllocSize cons)
constructorAllocSize (Tc.FunCons (_loc, _) _ tys : cons) = do
    let size = sizeOf opaquePtr * fromIntegral (length tys)
    rest <- constructorAllocSize cons
    pure (max (fromIntegral size) rest)


dsArg :: Tc.Arg Tc.Tc -> DsM Arg
dsArg (Tc.Arg NoExtField name ty) = Arg name <$> dsType ty


dsStmt :: Tc.StmtTc -> DsM TyExpr
dsStmt = \case
    Tc.SExpr NoExtField expr -> dsExpr expr


dsExpr :: Tc.ExprTc -> DsM TyExpr
dsExpr = \case
    Tc.Lit (_info, ty) lit -> do
        lit <- dsLit lit
        named $ typed ty lit
    Tc.Var (_info, ty, binding) name -> Typed <$> dsType ty <*> pure (Var (dsBound binding) name)
    Tc.BinOp (_, ty) l op r -> do
        l <- dsExpr l
        let op' = dsBinOp op
        r <- dsExpr r
        typed ty (BinOp l op' r)
    Tc.Prefix (_info, ty) op expr -> do
        let op' = dsPrefixOp op
        expr <- dsExpr expr
        typed ty (PrefixOp op' expr)
    Tc.App (_info, ty) l rs -> do
        l <- dsExpr l
        rs <- mapM dsExpr rs
        ty <- dsType ty
        named $ pure $ Typed ty (App l rs)
    Tc.Let info name expr -> do
        (list, expr) <- contextually $ dsExpr expr
        mapM_ emit list
        let letTy = view stmtType info
        let exprTy = view varType info
        exprTy <- dsType exprTy
        unnamed $ typed letTy (Let name exprTy (Just expr))
        unitGlobalVariable
    Tc.Ass (info, binding) name op expr -> do
        named $ typed (view stmtType info) =<< ass name (view varType info) binding op expr
    Tc.Ret (_info, ty) expr -> do
        expr <- mapM dsExpr expr
        unnamed $ typed ty (Return (fromMaybe unit expr))
        unitGlobalVariable
    Tc.EBlock NoExtField block@(Tc.Block (_, ty) _ _) -> do
        ty <- dsType ty
        var <- declare ty
        f <- ask
        block <- dsBlock f var block
        emits block
        pure (Typed ty $ Var Bound var)
    Tc.Break (_info, ty) mbExpr -> do
        f <- ask
        expr <- mapM dsExpr mbExpr
        case expr of
            Nothing -> do
                unnamed $ typed ty Break
                unitVarible
            Just expr -> do
                f expr
                unnamed $ typed ty Break
                unitGlobalVariable
    Tc.If (_info, ty) cond trueBlk mbFalseBlk -> do
        ty' <- dsType ty
        var <- declare ty'
        cond <- dsExpr cond
        f <- ask
        trueBlk <- dsBlock f var trueBlk
        mbFalseBlk <- mapM (dsBlock f var) mbFalseBlk
        unnamed $ typed ty (If cond trueBlk mbFalseBlk)
        named $ pure $ Typed ty' (Var Bound var)
    Tc.While (_info, ty) cond block -> do
        cond <- dsExpr cond
        ty' <- dsType ty
        var <- declare ty'
        block <- dsBlock (emit . Typed Unit . Ass var ty') var block
        unnamed $ typed ty (While cond block)
        named $ pure $ Typed Unit (Var Bound var)
    Tc.Loop (_info, ty) block -> do
        ty' <- dsType ty
        var <- declare ty'
        block <- dsBlock (emit . Typed Unit . Ass var ty') var block
        unnamed $ typed ty (While true block)
        named $ pure $ Typed ty' (Var Bound var)
    Tc.Lam (_info, ty) lamArgs body -> do
        args <- mapM mkArg lamArgs
        freshName <- fresh "lambda"
        ty' <- dsType ty
        returnType <- case ty of
            Tc.TyFun _ _ retty -> dsType retty
            nonFunTy -> __IMPOSSIBLE__
        (lambdaBody, expr) <- contextually $ dsExpr body
        modifying
            lifted
            ( `snoc`
                Fn
                    Origin.Lifted
                    freshName
                    args
                    returnType
                    (toList (lambdaBody `snoc` Typed returnType (Return expr)))
            )
        pure (Typed ty' (Var Function freshName))
    Tc.Match (loc, ty) scrutinee matchArms -> do
        ty <- dsType ty
        scrutinee <- dsExpr scrutinee
        matchArms <- dsMatchArms matchArms
        matchArms <- extractCatch loc matchArms
        named $ pure $ Typed ty $ uncurry (Match scrutinee) matchArms
    Tc.Expr (Tc.TypeApp expr _) -> dsExpr expr


-- TODO: Really reconsider if this is the logic we want
extractCatch :: SourceInfo -> [Either MatchArm Catch] -> DsM ([MatchArm], Catch)
extractCatch loc arms = (go mempty (lefts arms),) <$> maybe nonexhaustive pure (listToMaybe $ rights arms)
  where
    nonexhaustive :: DsM Catch
    nonexhaustive = do
        let errString = show loc <> "\\0A    Non-exhaustive pattern in match"
        name <- fresh "nonexhaustive_string"
        modifying staticStrings ((name, ArrayType (Text.length errString - 1) (I 8), errString <> "\\00") :)
        n <- use nameCounter
        nameCounter += 1
        pure
            $ Catch
                (Ident $ "$nomatch$" <> show n)
                (pure $ Typed Unit $ ToStderrExit name)
    go :: Set Int -> [MatchArm] -> [MatchArm]
    go _ [] = []
    go seen (arm@(MatchArm (PCon n _) _) : arms) =
        if Set.member n seen then go seen arms else arm : go (Set.insert n seen) arms


dsMatchArms :: [Tc.MatchArmTc] -> DsM [Either MatchArm Catch]
dsMatchArms [] = pure []
dsMatchArms (Tc.MatchArm _loc pat body : rest) = do
    pat <- dsPat pat
    body <-
        (\(ls, r) -> maybe (pure r) (<> pure r) $ nonEmpty (toList ls))
            <$> contextually (dsExpr body)
    case pat of
        Left catchAll -> pure [Right (Catch catchAll body)]
        Right pat -> (Left (MatchArm pat body) :) <$> dsMatchArms rest


dsPat :: Tc.PatternTc -> DsM (Either Ident Pattern)
dsPat = \case
    Tc.PVar _loc name -> pure $ Left name
    Tc.PEnumCon _loc name -> do
        index <- lookupCon name
        pure $ Right $ PCon index []
    Tc.PFunCon _loc name nestedPats -> do
        index <- lookupCon name
        let toVar (Tc.PVar (_, ty) name) = do
                ty <- dsType ty
                pure (name, ty)
            toVar _ = __IMPOSSIBLE__
        Right . PCon index <$> mapM toVar nestedPats


dsType :: (Monad m) => Tc.TypeTc -> m Type
dsType = \case
    Tc.Type (Mono _) -> pure OpaquePointer -- NOTE: Should be impossible
    Tc.TypeVar bound _ -> pure OpaquePointer
    Tc.TyCon NoExtField name -> pure (TyCon name)
    Tc.TyLit NoExtField Tc.Unit -> pure Unit
    Tc.TyLit NoExtField Tc.String -> pure (PointerType (I 8))
    Tc.TyLit NoExtField Tc.Char -> pure (I 8)
    Tc.TyLit NoExtField Tc.Double -> pure Float
    Tc.TyLit NoExtField Tc.Int -> pure (I 64)
    Tc.TyLit NoExtField Tc.Bool -> pure (I 1)
    Tc.TyFun _ l r -> do
        ls <- mapM dsType l
        r <- dsType r
        pure $ TyFun ls r
    Tc.Type Tc.AnyX -> pure Unit -- NOTE: `Any` is only the type
    -- of `return` and `break` so that they can be placed anywhere.


mkArg :: (Monad m) => Tc.LamArgTc -> m Arg
mkArg (Tc.LamArg ty name) = do
    ty <- dsType ty
    pure (Arg name ty)


-- TODO: Rewrite
dsBlock :: (TyExpr -> DsM ()) -> Ident -> Tc.Block Tc.Tc -> DsM [TyExpr]
dsBlock f _ (Tc.Block (_, Tc.TyLit NoExtField Tc.Unit) stmts tail) = do
    names' <- use names
    n <- use nameCounter
    cons <- use constructorIndex
    let ((), Env emits names'' n' lifteds strings _) =
            run cons f names' n $ mapM_ dsStmt (stmts <> maybe [] (\x -> [Tc.SExpr NoExtField x]) tail)
    assign names names''
    assign nameCounter n'
    modifying lifted (<> lifteds)
    modifying staticStrings (<> strings)
    pure (toList emits)
dsBlock f _ (Tc.Block _ stmts Nothing) = do
    names' <- use names
    n <- use nameCounter
    cons <- use constructorIndex
    let ((), Env emits names'' n' lifteds strings _) = run cons f names' n $ mapM_ dsStmt stmts
    assign names names''
    assign nameCounter n'
    modifying lifted (<> lifteds)
    modifying staticStrings (<> strings)
    pure (toList emits)
dsBlock f variable (Tc.Block (_, ty) stmts (Just tail)) = do
    names' <- use names
    n <- use nameCounter
    cons <- use constructorIndex
    let ((), Env emits names'' n' lifteds strings _) =
            run cons f names' n $ do
                ty <- dsType ty
                mapM_ dsStmt stmts
                tail <- dsExpr tail
                emit $ Typed Unit $ Ass variable ty tail
    assign names names''
    assign nameCounter n'
    modifying lifted (<> lifteds)
    modifying staticStrings (<> strings)
    pure (toList emits)


dsLit :: Tc.LitTc -> DsM Expr
dsLit = \case
    Tc.IntLit NoExtField int -> pure $ Constant $ IntLit int
    Tc.DoubleLit NoExtField double -> pure $ Constant $ DoubleLit double
    Tc.StringLit NoExtField string -> do
        name <- fresh "static_string"
        modifying staticStrings ((name, ArrayType (Text.length string + 1) (I 8), string <> "\\00") :)
        pure (Var Function name)
    Tc.CharLit NoExtField char -> pure $ Constant $ CharLit char
    Tc.BoolLit NoExtField bool -> pure $ Constant $ BoolLit bool
    Tc.UnitLit NoExtField -> pure $ Constant UnitLit


ass :: Ident -> Tc.TypeTc -> Bound.Boundedness -> Tc.AssignOp -> Tc.Expr Tc.Tc -> DsM Expr
ass name typ binding op xpr = do
    expr <- dsExpr xpr
    ty <- dsType typ
    let assignment operator =
            pure
                $ Ass
                    name
                    ty
                    (Typed ty $ BinOp (Typed ty $ Var (dsBound binding) name) operator expr)
     in case op of
            Tc.Assign -> pure $ Ass name ty expr
            Tc.DivAssign -> assignment Div
            Tc.MulAssign -> assignment Mul
            Tc.AddAssign -> assignment Add
            Tc.SubAssign -> assignment Sub
            Tc.ModAssign -> assignment Mod


true :: TyExpr
true = Typed (I 8) $ Constant $ BoolLit True


unit :: TyExpr
unit = Typed (I 1) $ Constant UnitLit


typed :: Tc.TypeTc -> Expr -> DsM TyExpr
typed ty expr = Typed <$> dsType ty <*> pure expr


lookupCon :: Ident -> DsM Int
lookupCon name = uses constructorIndex (fromJust . Map.lookup name)


dsPrefixOp :: Tc.PrefixOp -> PrefixOp
dsPrefixOp = \case
    Tc.Not -> Not
    Tc.Neg -> Neg


dsBinOp :: Tc.BinOp -> BinOp
dsBinOp = \case
    Tc.Add -> Add
    Tc.Sub -> Sub
    Tc.Mul -> Mul
    Tc.Div -> Div
    Tc.Mod -> Mod
    Tc.Or -> Or
    Tc.And -> And
    Tc.Lt -> Lt
    Tc.Gt -> Gt
    Tc.Lte -> Lte
    Tc.Gte -> Gte
    Tc.Eq -> Eq
    Tc.Neq -> Neq


dsBound :: Bound.Boundedness -> Binding
dsBound = \case
    Bound.Free -> Free
    Bound.Bound -> Bound
    Bound.Function -> Function
    Bound.Constructor -> Constructor
    Bound.Builtin -> Function
    Bound.Imported -> Function


contextually :: DsM a -> DsM (DList TyExpr, a)
contextually m = do
    exprs <- use expressions
    assign expressions mempty
    e <- m
    emits <- use expressions
    assign expressions exprs
    pure (emits, e)
