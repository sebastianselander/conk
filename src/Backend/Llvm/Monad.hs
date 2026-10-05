{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Backend.Llvm.Monad where

import Control.Lens (makeLenses)
import Control.Lens.Getter (use, uses)
import Control.Lens.Setter (assign, modifying, (+=))
import Data.DList (DList, snoc)
import Relude hiding (Type)

import Data.Set qualified as Set
import Data.Text qualified as Text

import Backend.Llvm.Types
import Backend.Types
import Impossible (__IMPOSSIBLE__)
import Names (Ident (..))


data IRBuilderState = IRBuilderState
    { _instructions :: DList (Named Instruction)
    , _varCounter :: !Word16
    , _labelCounter :: !Word16
    , _labelReserved :: Set Ident
    , _constructors :: Map Ident Int
    , _predBlock :: Label
    }
    deriving (Show)


newtype IRBuilderCtx = IRBuilderCtx
    { _breakLabel :: Label
    }


$(makeLenses ''IRBuilderState)
$(makeLenses ''IRBuilderCtx)


emptyInstructions :: DList (Named Instruction)
emptyInstructions = mempty


entryBlock :: Label
entryBlock = L (Ident "entry")


initialIRBuilderState :: IRBuilderState
initialIRBuilderState =
    IRBuilderState
        { _instructions = mempty
        , _varCounter = 0
        , _labelCounter = 1
        , _labelReserved = mempty
        , _constructors = mempty
        , _predBlock = entryBlock
        }


initialIRBuilderCtx :: IRBuilderCtx
initialIRBuilderCtx = IRBuilderCtx {_breakLabel = entryBlock}


newtype IRBuilder a = IRBuilder {runBuilder :: StateT IRBuilderState (Reader IRBuilderCtx) a}
    deriving (Applicative, Functor, Monad, MonadReader IRBuilderCtx, MonadState IRBuilderState)


inContext :: IRBuilder a -> IRBuilder (a, [Named Instruction])
inContext ma = do
    before <- getInstructions
    clearInstructions
    a <- ma
    during <- getInstructions
    setInstructions (fromList before)
    pure (a, during)


clearInstructions :: IRBuilder ()
clearInstructions = assign instructions emptyInstructions


setInstructions :: DList (Named Instruction) -> IRBuilder ()
setInstructions = assign instructions


getInstructions :: IRBuilder [Named Instruction]
getInstructions = uses instructions toList


-- | Get current instruction list, clear it, and then return the gotten list.
extractInstructions :: IRBuilder [Named Instruction]
extractInstructions = do
    instrs <- getInstructions
    clearInstructions
    pure instrs


runAssembler :: IRBuilder a -> a
runAssembler = flip runReader initialIRBuilderCtx . flip evalStateT initialIRBuilderState . runBuilder


emit :: Named Instruction -> IRBuilder ()
emit instr = modifying instructions (`snoc` instr)


fresh :: (MonadState IRBuilderState m) => m Ident
fresh = do
    n <- use varCounter
    varCounter += 1
    pure $ Ident ("_" <> show n)


call :: Type -> Operand -> [Operand] -> IRBuilder Operand
call ty function args = LocalReference ty <$> named (Call ty function args)


voidCall :: Operand -> [Operand] -> IRBuilder ()
voidCall function args = emit $ Nameless $ Call Unit function args


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
arith op ty l r = LocalReference ty <$> named (Arith op ty l r)


named :: Instruction -> IRBuilder Ident
named instr = do
    name <- fresh
    emit $ Named name instr
    pure name


unnamed :: Instruction -> IRBuilder ()
unnamed instr = emit (Nameless instr)


cmp :: CmpOp -> Type -> Operand -> Operand -> IRBuilder Operand
cmp op ty l r = LocalReference ty <$> named (Cmp op ty l r)


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
and ty l r = LocalReference ty <$> named (And ty l r)


or :: Type -> Operand -> Operand -> IRBuilder Operand
or ty l r = LocalReference ty <$> named (Or ty l r)


alloca :: Ident -> Type -> IRBuilder Operand
alloca name ty = do
    emit $ Named name (Alloca ty)
    pure $ LocalReference (ptr ty) name


malloc :: Type -> Operand -> IRBuilder Operand
malloc ty operand = do
    name <- fresh
    emit $ Named name (Malloc operand)
    pure $ LocalReference ty name


store :: Operand -> Operand -> IRBuilder ()
store lop rop = unnamed (Store lop rop)


load :: Type -> Operand -> IRBuilder Operand
load ty operand = LocalReference ty <$> named (Load operand)


ret :: Operand -> IRBuilder ()
ret operand = unnamed (Ret operand)


label :: Label -> IRBuilder ()
label lbl = do
    assign predBlock lbl
    emit (Nameless $ Label lbl)


-- TODO: Fix numbering
mkLabel :: Text -> IRBuilder Label
mkLabel desc =
    if ' ' `Text.elem` desc
        then
            go "" >>= \case
                L (Ident name) -> pure $ L (Ident $ "\"" <> name <> "\"")
        else go ""
  where
    go suffix = do
        let lbl = Ident $ desc <> suffix
        used <- use labelReserved
        if lbl `Set.member` used
            then do
                n <- use labelCounter
                labelCounter += 1
                go ("." <> show n)
            else modifying labelReserved (Set.insert lbl) >> pure (L lbl)


comment :: Text -> IRBuilder ()
comment cmnt = emit (Nameless (Comment cmnt))


br :: Operand -> Label -> Label -> IRBuilder ()
br operand leftLbl rightLbl = unnamed (Br operand leftLbl rightLbl)


jump :: Label -> IRBuilder ()
jump lbl = unnamed (Jump lbl)


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
switch op l xs = unnamed (Switch op l xs)


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


global :: Type -> Ident -> Operand
global ty = ConstantOperand . GlobalReference ty


constant :: Constant -> Operand
constant = ConstantOperand


struct :: [Constant] -> Operand
struct = ConstantOperand . LStruct


null :: Type -> Operand
null ty = ConstantOperand (LNull ty)


localRef :: Type -> Ident -> Operand
localRef = LocalReference


blankline :: IRBuilder ()
blankline = unnamed Blankline


undef :: Type -> Operand
undef = ConstantOperand . Undef
