module Backend.Llvm.Llvm2 where

import Data.DList (DList, snoc)
import Data.Maybe (fromJust)
import Relude hiding (Type, and, div, or, rem)
import Text.Pretty.Simple (pShow)

import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Text.IO qualified as Text

import Backend.Core.Pretty (pExpr)
import Backend.Llvm.Monad (entryLabel, labelReserved)
import Backend.Llvm.Prelude (prelude)
import Backend.Llvm.Types
    ( ArithOp (..),
      BasicBlock,
      CmpOp (..),
      Constant (..),
      Definition (..),
      Ellipsis (NoEllipsis),
      Instruction (..),
      Label (L, labelName),
      Module (..),
      Named (..),
      Operand (..),
      PartialBlock (..),
      Terminator (Br, Jump, Ret),
      emptyPartialBlock,
      finalizePartialBlock,
      typeOf,
    )
import Backend.Types (Type (..), ptr, sizeOf)
import Impossible (__IMPOSSIBLE__)
import Names (ExplicitName (..), Ident (..), Namespace)
import Utils (mapWithIndexM)

import Backend.Core.Types qualified as Core
import Origin qualified

type Bindings = Map ExplicitName Operand

data IRState = IRState
    { builderBlock :: Maybe PartialBlock
    , blocks :: DList BasicBlock
    , bindings :: Bindings
    , nameCounter :: Int
    , namespace :: Namespace
    , breakLabel :: Label
    , reservedLabels :: Set Label
    , labelCounter :: Int
    }

emptyIRState :: Namespace -> Bindings -> IRState
emptyIRState namespace bindings =
    IRState
        { builderBlock = Nothing
        , blocks = mempty
        , bindings = bindings
        , nameCounter = 0
        , namespace = namespace
        , breakLabel = entryLabel
        , reservedLabels = mempty
        , labelCounter = 0
        }

type IRBuilder a = (ReaderT (IORef IRState) IO) a

lookupBinding :: (HasCallStack) => ExplicitName -> IRBuilder Operand
lookupBinding name = do
    ref <- ask
    op <- Map.lookup name . bindings <$> readIORef ref
    pure $ fromMaybe (error $ "Could not find: " <> show name) op

insertBinding :: ExplicitName -> Operand -> IRBuilder ()
insertBinding name operand = do
    ref <- ask
    modifyIORef ref (\s -> s {bindings = Map.insert name operand (bindings s)})

assemble :: Core.Program -> IO (NonEmpty Module)
assemble (Core.Program modules) = traverse assembleModule modules

assembleModule :: Core.Module -> IO Module
assembleModule mod@(Core.Module namespace defs) = do
    let bindings = createBindings mod
    state <- newIORef (emptyIRState namespace bindings)
    defs <- runReaderT (traverse assembleDefinition defs) state
    pure $ Module defs

withBlock :: Label -> IRBuilder (Terminator, b) -> IRBuilder (DList BasicBlock, b)
withBlock lbl action = do
    ref <- ask
    state <- readIORef ref
    tmpRef <- newIORef (state {builderBlock = Just (emptyPartialBlock lbl), blocks = mempty})
    local (const tmpRef) do
        (term, b) <- action
        state <- readIORef tmpRef
        let bb = finalizePartialBlock (Nameless term) (fromJust state.builderBlock)
        pure (state.blocks `snoc` bb, b)

createBindings :: Core.Module -> Bindings
createBindings (Core.Module namespace defs) = foldr get preludeBindings defs
  where
    -- preludeBindings :: Bindings
    preludeBindings = Map.fromList $ fst <$> fst prelude
    get :: Core.Def -> Bindings -> Bindings
    get def bindings = case def of
        Core.Decl _ ty name _ -> Map.insert name (ConstantOperand (GlobalReference ty name)) bindings
        Core.Fn _ name args retTy _ ->
            Map.insert name (ConstantOperand (GlobalReference (TyFun (fmap typeOf args) retTy) name)) bindings
        Core.Main _ _ -> bindings
        Core.StaticString name ty _ -> Map.insert name (ConstantOperand (GlobalReference OpaquePointer name)) bindings
        Core.TypeSyn _ _ -> undefined
        Core.Con _ name ty _ -> Map.insert name (ConstantOperand (GlobalReference ty name)) bindings

assembleDefinition :: Core.Def -> IRBuilder Definition
assembleDefinition def = case def of
    Core.Fn origin name arguments returnType block -> do
        ref <- ask
        modifyIORef ref $ \state -> state {builderBlock = Nothing, blocks = mempty}
        openBlock entryLabel
        arguments <- traverse assembleArgument arguments
        blocks <- assembleExpressions block
        emitBlock
        pure (Function origin name arguments returnType blocks)
    Core.Main namespace block -> do
        ref <- ask
        modifyIORef ref $ \state -> state {builderBlock = Nothing, blocks = mempty}
        openBlock entryLabel
        let name = EName namespace "main" "main"
        blocks <- assembleExpressions block
        pure (Function Origin.Function name [] (I 1) blocks)
    Core.Decl namespace ty name args -> pure (Declare ty name args NoEllipsis)
    Core.StaticString name ty txt -> pure (GlobalString name ty txt)
    Core.TypeSyn name ty -> pure (TypeDefinition name ty)
    Core.Con index name ty args -> assembleConstructor index name ty args

assembleConstructor :: Int -> ExplicitName -> Type -> [Type] -> IRBuilder Definition
assembleConstructor index name ty args = do
    (blocks, (args, retTy)) <- withBlock entryLabel $ do
        args <- mapM (\ty -> LocalReference ty <$> fresh) args
        let retTy = getReturnType ty
        name' <- fresh
        returnOperand <- alloca name' retTy
        tag <- gep returnOperand [i32 @Int 0, i32 @Int 0]
        store (i64 index) tag
        void $ flip mapWithIndexM args $ \index argument -> do
            value <- gep returnOperand [i32 @Integer 0, i32 @Integer 1, i32 @Integer index]
            -- TODO: Do one malloc for all arguments?
            malloced <- malloc (ptr (typeOf argument)) (i64 (sizeOf (typeOf argument)))
            -- TODO(sebsel): Is this GEP needed?
            mallocedPtr <- gep malloced [i32 @Integer 0]
            store argument mallocedPtr
            store malloced value
        struct <- load returnOperand
        term <- ret struct
        pure (term, (args, retTy))
    pure (Function Origin.ConstructorFn name args retTy (toList blocks))

assembleArgument :: Core.Arg -> IRBuilder Operand
assembleArgument (Core.Arg name ty) = do
    let newName = name {name = mkArgName name.name}
    let arg = LocalReference ty newName
    operand <- alloca name ty
    store arg operand
    insertBinding name operand
    pure arg
assembleArgument (Core.EnvArg namespace ty) = pure (LocalReference ty (EName namespace env env))
  where
    env = Ident "env"

assembleExpressions :: [Core.TyExpr] -> IRBuilder [BasicBlock]
assembleExpressions exprs = do
    local id $ do
        traverse_ assembleExpression exprs
        ref <- ask
        state <- readIORef ref
        let block = finalizePartialBlock (Nameless (Ret Nothing)) <$> state.builderBlock
        modifyIORef ref $ \state -> state {builderBlock = Nothing}
        case block of
            Nothing -> pure (toList state.blocks)
            Just block -> pure (toList $ state.blocks `snoc` block)

openBlock :: Label -> IRBuilder ()
openBlock lbl = do
    ref <- ask
    modifyIORef ref $ \s -> s {builderBlock = Just $ emptyPartialBlock lbl}

emitBlockStart :: Label -> IRBuilder ()
emitBlockStart lbl = emitBlock >> openBlock lbl

currentBlock :: (HasCallStack) => IRBuilder Label
currentBlock = do
    ref <- ask
    state <- readIORef ref
    case state.builderBlock of
        Nothing -> error "No block has been started"
        Just partialBlock -> pure partialBlock.partialLabel

emitBlock :: IRBuilder ()
emitBlock = do
    ref <- ask
    state <- readIORef ref
    let state' =
            state
                { builderBlock = Nothing
                , blocks =
                    maybe
                        state.blocks
                        (\block -> state.blocks `snoc` finalizePartialBlock (Nameless $ Ret Nothing) block)
                        state.builderBlock
                }
    writeIORef ref state'

assembleLoadExpression :: Core.TyExpr -> IRBuilder Operand
assembleLoadExpression expr = do
    operand <- assembleExpression expr
    case operand of
        LocalReference {} -> load operand
        ConstantOperand {} -> pure operand

assembleExpression :: (HasCallStack) => Core.TyExpr -> IRBuilder Operand
assembleExpression (Core.Typed ty expr) =
    -- To ensure we're always inside a block
    currentBlock >> case expr of
        Core.Constant lit -> do
            pure (assembleLiteral lit)
        Core.Var binding name -> do
            lookupBinding name
        Core.BinOp leftExpr operator rightExpr -> do
            leftExpr <- assembleLoadExpression leftExpr
            rightExpr <- assembleLoadExpression rightExpr
            llvmBinOp operator ty leftExpr rightExpr
        Core.PrefixOp Core.Deref expr -> undefined
        Core.PrefixOp Core.Ref expr -> undefined
        Core.PrefixOp Core.Not expr -> undefined
        Core.PrefixOp Core.Neg expr -> undefined
        Core.App appExpr argExprs -> do
            app <- assembleLoadExpression appExpr
            args <- mapM assembleLoadExpression argExprs
            call ty app args
        Core.Let name varType Nothing -> do
            operand <- alloca name varType
            insertBinding name operand
            pure operand
        Core.Let name varType (Just expr) -> do
            operand <- alloca name varType
            value <- assembleLoadExpression expr
            insertBinding name operand
            () <- store value operand
            pure operand
        Core.DerefAss name varType expr -> undefined
        Core.Ass name varType expr -> do
            expr <- assembleLoadExpression expr
            operand <- lookupBinding name
            store expr operand
            unit
        Core.Return expr -> do
            operand <- assembleLoadExpression expr
            res <- ret operand
            pure (poison (typeOf operand))
        Core.Break -> do
            state <- ask >>= readIORef
            _ <- jump state.breakLabel
            unit
        Core.If condition trueBlk falseBlk -> do
            let true = L "if_true"
            let false = L "if_false"
            let end = L "if_cont"
            condition <- assembleLoadExpression condition
            _ <- br condition true (maybe end (const false) falseBlk)
            emitBlockStart true
            mapM_ assembleExpression trueBlk
            _ <- jump end
            emitBlockStart false
            maybe (pure ()) (mapM_ assembleExpression) falseBlk
            _ <- jump end
            emitBlockStart end
            unit -- hacky
        Core.While condition blk -> do
            start <- mkLabel "while_start"
            continue <- mkLabel "while_continue"
            end <- mkLabel "while_end"
            _ <- jump continue
            emitBlockStart continue
            condition <- assembleLoadExpression condition
            _ <- br condition start end
            emitBlockStart start
            ref <- ask
            state <- readIORef ref
            writeIORef ref (state {breakLabel = end})
            ops <- mapM assembleExpression blk
            liftIO $ print ops
            writeIORef ref (state {breakLabel = state.breakLabel})
            _ <- jump continue
            emitBlockStart end
            unit
        Core.Closure fun env -> undefined
        Core.StructIndexing expr n -> undefined
        Core.ExtractFree bindName envName index -> undefined
        Core.Match scrutinee@(Core.Typed _ _) matchArms (Core.Catch _name catchExpr) -> undefined
        Core.ToStderrExit var -> undefined

assembleLiteral :: Core.Literal -> Operand
assembleLiteral = \case
    Core.IntLit int -> ConstantOperand (LInt (I 64) int)
    Core.DoubleLit double -> ConstantOperand (LDouble Float double)
    Core.CharLit char -> ConstantOperand (LChar (I 8) char)
    Core.BoolLit bool -> ConstantOperand (LBool (I 1) bool)
    Core.UnitLit -> ConstantOperand LUnit
    Core.NullLit -> ConstantOperand (LNull (PointerType Void))

llvmBinOp :: Core.BinOp -> (Type -> Operand -> Operand -> IRBuilder Operand)
llvmBinOp op = case op of
    Core.MulOp -> mul
    Core.DivOp -> div
    Core.AddOp -> add
    Core.SubOp -> sub
    Core.ModOp -> rem
    Core.LtOp -> lt
    Core.OrOp -> or
    Core.AndOp -> and
    Core.GtOp -> gt
    Core.LteOp -> le
    Core.GteOp -> ge
    Core.EqOp -> eq
    Core.NeqOp -> neq

jump :: Label -> IRBuilder Terminator
jump lbl = emitTerminator (Jump lbl)

ret :: Operand -> IRBuilder Terminator
ret operand = do
    emitTerminator (Ret $ Just operand)

and :: Type -> Operand -> Operand -> IRBuilder Operand
and ty l r = do
    name <- fresh
    operand <- alloca name ty
    name <- named (And ty l r)
    store (LocalReference ty name) operand
    pure operand

unnamed :: Instruction -> IRBuilder ()
unnamed instr = emitInstruction (Nameless instr)

store :: Operand -> Operand -> IRBuilder ()
store lop rop = unnamed (Store lop rop)

or :: Type -> Operand -> Operand -> IRBuilder Operand
or ty l r = do
    name <- fresh
    operand <- alloca name ty
    name <- named (Or ty l r)
    store (LocalReference ty name) operand
    pure operand

add :: Type -> Operand -> Operand -> IRBuilder Operand
add = arith LlvmAdd

sub :: Type -> Operand -> Operand -> IRBuilder Operand
sub = arith LlvmSub

mul :: Type -> Operand -> Operand -> IRBuilder Operand
mul = arith LlvmMul

div :: Type -> Operand -> Operand -> IRBuilder Operand
div = arith LlvmDiv

rem :: Type -> Operand -> Operand -> IRBuilder Operand
rem = arith LlvmRem

eq :: Type -> Operand -> Operand -> IRBuilder Operand
eq = cmp LlvmEq

neq :: Type -> Operand -> Operand -> IRBuilder Operand
neq = cmp LlvmNeq

gt :: Type -> Operand -> Operand -> IRBuilder Operand
gt = cmp LlvmGt

lt :: Type -> Operand -> Operand -> IRBuilder Operand
lt = cmp LlvmLt

ge :: Type -> Operand -> Operand -> IRBuilder Operand
ge = cmp LlvmGe

le :: Type -> Operand -> Operand -> IRBuilder Operand
le = cmp LlvmLe

emitTerminator :: Terminator -> IRBuilder Terminator
emitTerminator term = do
    ref <- ask
    state <- readIORef ref
    let builderBlock = state.builderBlock
    case builderBlock of
        Nothing -> pure ()
        Just builderBlock ->
            modifyIORef ref $ \s ->
                s
                    { builderBlock =
                        Just
                            $ builderBlock
                                { partialTerminator = Just $ fromMaybe (Nameless term) builderBlock.partialTerminator
                                }
                    }
    pure term

emitInstruction :: Named Instruction -> IRBuilder ()
emitInstruction instr = do
    ref <- ask
    state <- readIORef ref
    let builderBlock = state.builderBlock
    case builderBlock of
        Nothing -> pure ()
        Just builderBlock ->
            modifyIORef ref $ \s ->
                s
                    { builderBlock =
                        Just
                            $ builderBlock
                                { partialInstructions = builderBlock.partialInstructions `snoc` instr
                                }
                    }

alloca :: ExplicitName -> Type -> IRBuilder Operand
alloca name ty = do
    emitInstruction $ Named name (Alloca ty)
    pure $ LocalReference (ptr ty) name

named :: Instruction -> IRBuilder ExplicitName
named instr = do
    name <- fresh
    emitInstruction $ Named name instr
    pure name

arith :: ArithOp -> Type -> Operand -> Operand -> IRBuilder Operand
arith op ty l r = do
    name <- fresh
    operand <- alloca name ty
    name <- named (Arith op ty l r)
    store (LocalReference ty name) operand
    pure operand

fresh :: IRBuilder ExplicitName
fresh = do
    ref <- ask
    state <- readIORef ref
    modifyIORef ref (\s -> s {nameCounter = nameCounter s + 1})
    let ident = Ident $ "_" <> show state.nameCounter
    pure $ EName state.namespace ident ident

cmp :: CmpOp -> Type -> Operand -> Operand -> IRBuilder Operand
cmp op ty l r = do
    name <- fresh
    operand <- alloca name ty
    name <- named (Cmp op ty l r)
    store (LocalReference ty name) operand
    pure operand

load :: Operand -> IRBuilder Operand
load operand = LocalReference retty <$> named (Load operand)
  where
    retty = case typeOf operand of
        PointerType ty -> ty
        _ -> error "Cannot load non-pointer operand"

call :: Type -> Operand -> [Operand] -> IRBuilder Operand
call ty function args = do
    name <- fresh
    operand <- alloca name ty
    name <- named (Call ty function args)
    store (LocalReference ty name) operand
    pure operand

poison :: Type -> Operand
poison = ConstantOperand . Poison

mkArgName :: Ident -> Ident
mkArgName (Ident name) = Ident $ name <> ".arg"

getReturnType :: Type -> Type
getReturnType = \case
    TyFun _ ty -> ty
    _ -> __IMPOSSIBLE__

i32 :: (Integral a) => a -> Operand
i32 = ConstantOperand . LInt (I 32) . fromIntegral

i64 :: (Integral a) => a -> Operand
i64 = ConstantOperand . LInt (I 32) . fromIntegral

--

-- | Does not work correctly for structure if gep is used nested
gep :: Operand -> [Operand] -> IRBuilder Operand
gep op ops = LocalReference (gepType (typeOf op) ops) <$> named (GetElementPtr op ops)

gepType :: Type -> [Operand] -> Type
gepType ty [] = ptr ty
gepType OpaquePointer _ = OpaquePointer
gepType (PointerType ty) (_ : is) = gepType ty is
gepType (StructType ty) ((ConstantOperand (LInt (I 32) n) : is)) = case maybeAt (fromIntegral n) ty of
    Nothing -> __IMPOSSIBLE__
    Just ty -> gepType ty is
gepType (StructType _ty) (i : _) = __IMPOSSIBLE__
gepType (TyCon _) _ = OpaquePointer
gepType ty (_ : _) = __IMPOSSIBLE__

malloc :: Type -> Operand -> IRBuilder Operand
malloc ty operand = do
    name <- fresh
    emitInstruction $ Named name (Malloc operand)
    pure $ LocalReference ty name

br :: Operand -> Label -> Label -> IRBuilder Terminator
br operand leftLbl rightLbl = emitTerminator (Br operand leftLbl rightLbl)

unit :: (Monad m) => m Operand
unit = pure $ ConstantOperand LUnit

copyModifyIORef :: (MonadIO m) => IORef a -> (a -> b) -> m (IORef b)
copyModifyIORef ref f = do
    a <- readIORef ref
    newIORef (f a)

mkLabel :: Text -> IRBuilder Label
mkLabel desc = do
    ref <- ask
    state <- readIORef ref
    (lbl, n) <- go state.labelCounter ""
    modifyIORef ref $ \state -> state {labelCounter = n, reservedLabels = Set.insert lbl state.reservedLabels}
    if Text.elem ' ' desc
        then pure (lbl {labelName = "\"" <> lbl.labelName <> "\""})
        else pure lbl
  where
    go n suffix = do
        let lbl = L $ desc <> suffix
        ref <- ask
        state <- readIORef ref
        let used = state.reservedLabels
        if lbl `Set.member` used
            then do
                go (n + 1) ("." <> show n)
            else pure (lbl, n)

class Value a where
    value :: a -> Either Terminator Operand

instance Value Operand where
    value = Right

instance Value Terminator where
    value = Left
