{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Backend.Llvm.Monad where

import Control.Lens (makeLenses, view)
import Control.Lens.Getter (use, uses)
import Control.Lens.Setter (assign, modifying, set, (+=))
import Data.DList (DList, snoc)
import Relude hiding (Type)

import Data.Set qualified as Set
import Data.Text qualified as Text

import Backend.Llvm.Types
import Backend.Types
import Impossible (__IMPOSSIBLE__)
import Names (ExplicitName (EName), Ident (..), Namespace)

data IRBuilderState = IRBuilderState
    { _builderBlock :: PartialBlock
    , _blocks :: DList BasicBlock
    , _varCounter :: !Word16
    , _labelCounter :: !Word16
    , _labelReserved :: Set Label
    , _constructors :: Map ExplicitName Int
    , _predBlock :: Label
    }
    deriving (Show)

data IRBuilderCtx = IRBuilderCtx
    { _breakLabel :: Label
    , _namespace :: Namespace
    }

$(makeLenses ''IRBuilderState)
$(makeLenses ''IRBuilderCtx)

emptyInstructions :: DList (Named Instruction)
emptyInstructions = mempty

entryLabel :: Label
entryLabel = L "entry"

initialIRBuilderState :: IRBuilderState
initialIRBuilderState =
    IRBuilderState
        { _builderBlock = emptyPartialBlock entryLabel
        , _blocks = mempty
        , _varCounter = 0
        , _labelCounter = 1
        , _labelReserved = mempty
        , _constructors = mempty
        , _predBlock = entryLabel
        }

initialIRBuilderCtx :: Namespace -> IRBuilderCtx
initialIRBuilderCtx namespace = IRBuilderCtx {_breakLabel = entryLabel, _namespace = namespace}

newtype IRBuilder a = IRBuilder {runBuilder :: StateT IRBuilderState (Reader IRBuilderCtx) a}
    deriving (Applicative, Functor, Monad, MonadReader IRBuilderCtx, MonadState IRBuilderState)

inContext :: IRBuilder a -> IRBuilder (a, [BasicBlock])
inContext ma = do
    before <- getBlocks
    clearBlocks
    a <- ma
    during <- getBlocks
    setBlocks (fromList before)
    pure (a, during)

clearBlocks :: IRBuilder ()
clearBlocks = do
    assign blocks mempty
    assign builderBlock (emptyPartialBlock entryLabel)

emitBlock :: IRBuilder ()
emitBlock = do
    partialBlock <- use builderBlock
    let instrs = toList partialBlock.partialInstructions
    let blk =
            BasicBlock
                { terminator = Nameless $ Ret Nothing
                , label = partialBlock.partialLabel
                , instructions = instrs
                }
    modifying blocks (`snoc` blk)

setBlocks :: DList BasicBlock -> IRBuilder ()
setBlocks = assign blocks

getBlocks :: IRBuilder [BasicBlock]
getBlocks = uses blocks toList

runAssembler :: Namespace -> IRBuilder a -> a
runAssembler namespace =
    flip runReader (initialIRBuilderCtx namespace) . flip evalStateT initialIRBuilderState . runBuilder

emitInstruction :: Named Instruction -> IRBuilder ()
emitInstruction instr = modifying builderBlock $ \block -> block {partialInstructions = block.partialInstructions `snoc` instr}

emitTerminator :: Named Terminator -> IRBuilder ()
emitTerminator term = undefined -- modifying builderBlock $ \block -> block {partialTerminator = Just $ fromMaybe term block.partialTerminator}

block :: Label -> IRBuilder ()
block lbl = do
    emitBlock
    modifying builderBlock (const (emptyPartialBlock lbl))

fresh :: (MonadReader IRBuilderCtx m, MonadState IRBuilderState m) => m ExplicitName
fresh = do
    n <- use varCounter
    varCounter += 1
    namespace <- view namespace
    pure $ EName namespace (Ident ("_" <> show n)) (Ident ("_" <> show n))

call :: Type -> Operand -> [Operand] -> IRBuilder Operand
call ty function args = do
    name <- fresh
    operand <- alloca name ty
    name <- named (Call ty function args)
    store (LocalReference ty name) operand
    pure operand

voidCall :: Operand -> [Operand] -> IRBuilder ()
voidCall function args = emitInstruction $ Nameless $ Call Unit function args

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

arith :: ArithOp -> Type -> Operand -> Operand -> IRBuilder Operand
arith op ty l r = do
    name <- fresh
    operand <- alloca name ty
    name <- named (Arith op ty l r)
    store (LocalReference ty name) operand
    pure operand

named :: Instruction -> IRBuilder ExplicitName
named instr = do
    name <- fresh
    emitInstruction $ Named name instr
    pure name

unnamed :: Instruction -> IRBuilder ()
unnamed instr = emitInstruction (Nameless instr)

cmp :: CmpOp -> Type -> Operand -> Operand -> IRBuilder Operand
cmp op ty l r = do
    name <- fresh
    operand <- alloca name ty
    name <- named (Cmp op ty l r)
    store (LocalReference ty name) operand
    pure operand

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

and :: Type -> Operand -> Operand -> IRBuilder Operand
and ty l r = do
    name <- fresh
    operand <- alloca name ty
    name <- named (And ty l r)
    store (LocalReference ty name) operand
    pure operand

or :: Type -> Operand -> Operand -> IRBuilder Operand
or ty l r = do
    name <- fresh
    operand <- alloca name ty
    name <- named (Or ty l r)
    store (LocalReference ty name) operand
    pure operand

alloca :: ExplicitName -> Type -> IRBuilder Operand
alloca name ty = do
    emitInstruction $ Named name (Alloca ty)
    pure $ LocalReference (ptr ty) name

malloc :: Type -> Operand -> IRBuilder Operand
malloc ty operand = do
    name <- fresh
    emitInstruction $ Named name (Malloc operand)
    pure $ LocalReference ty name

store :: Operand -> Operand -> IRBuilder ()
store lop rop = unnamed (Store lop rop)

unreachable :: IRBuilder ()
unreachable = emitTerminator (Nameless Unreachable)

load :: Type -> Operand -> IRBuilder Operand
load ty operand = LocalReference ty <$> named (Load operand)

ret :: Operand -> IRBuilder ()
ret operand = emitTerminator (Nameless $ Ret $ Just operand)

-- TODO: Fix numbering
mkLabel :: Text -> IRBuilder Label
mkLabel desc =
    if ' ' `Text.elem` desc
        then
            go "" >>= \case
                L name -> pure $ L ("\"" <> name <> "\"")
        else go ""
  where
    go suffix = do
        let lbl = L $ desc <> suffix
        used <- use labelReserved
        if lbl `Set.member` used
            then do
                n <- use labelCounter
                labelCounter += 1
                go ("." <> show n)
            else modifying labelReserved (Set.insert lbl) >> pure lbl

comment :: Text -> IRBuilder ()
comment cmnt = emitInstruction (Nameless (Comment cmnt))

br :: Operand -> Label -> Label -> IRBuilder ()
br operand leftLbl rightLbl = emitTerminator (Nameless $ Br operand leftLbl rightLbl)

jump :: Label -> IRBuilder ()
jump lbl = do
    comment "Jump"
    emitTerminator (Nameless $ Jump lbl)

-- | Does not work correctly for structure if gep is used nested
gep :: Operand -> [Operand] -> IRBuilder Operand
gep op ops = LocalReference (gepType (typeOf op) ops) <$> named (GetElementPtr op ops)

extractValue :: Maybe Type -> Operand -> [Word32] -> IRBuilder Operand
extractValue mbty operand indices =
    LocalReference (fromMaybe (extractValueType (typeOf operand) indices) mbty)
        <$> named (ExtractValue operand indices)

phi :: [(Operand, Label)] -> IRBuilder Operand
phi [] = LocalReference Void <$> named (Phi [])
phi xs@(i : _) = LocalReference (typeOf (fst i)) <$> named (Phi xs)

switch :: Operand -> Label -> [(Constant, Label)] -> IRBuilder ()
switch op l xs = emitTerminator (Nameless $ Switch op l xs)

gepType :: Type -> [Operand] -> Type
gepType ty [] = ptr ty
gepType OpaquePointer _ = OpaquePointer
gepType (PointerType ty) (_ : is) = gepType ty is
gepType (StructType ty) ((ConstantOperand (LInt Int32 n) : is)) = case maybeAt (fromIntegral n) ty of
    Nothing -> __IMPOSSIBLE__
    Just ty -> gepType ty is
gepType (StructType _ty) (i : _) = __IMPOSSIBLE__
gepType (TyCon _) _ = OpaquePointer
gepType ty (_ : _) = __IMPOSSIBLE__

extractValueType :: Type -> [Word32] -> Type
extractValueType ty [] = ty
extractValueType ty (x : xs) = case ty of
    StructType tys -> case maybeAt (fromIntegral x) tys of
        Nothing -> __IMPOSSIBLE__
        Just ty -> extractValueType ty xs
    ty -> __IMPOSSIBLE__

global :: Type -> ExplicitName -> Operand
global ty = ConstantOperand . GlobalReference ty

constant :: Constant -> Operand
constant = ConstantOperand

struct :: [Constant] -> Operand
struct = ConstantOperand . LStruct

null :: Type -> Operand
null ty = ConstantOperand (LNull ty)

localRef :: Type -> ExplicitName -> Operand
localRef = LocalReference

blankline :: IRBuilder ()
blankline = unnamed Blankline

undef :: Type -> Operand
undef = ConstantOperand . Undef

poison :: Type -> Operand
poison = ConstantOperand . Poison
