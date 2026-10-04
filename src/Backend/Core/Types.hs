module Backend.Core.Types where

import Data.Data (Data)
import Relude hiding (Type)

import Backend.Llvm.Types (Operand (LocalReference))
import Backend.Types (Type (..))
import Names (Ident, Namespace)
import Origin (Origin (..))


newtype Program = Program [Def]
    deriving (Data, Eq, Ord, Show)


data Def
    = Decl Namespace Type Ident [Type]
    | Fn !Origin Ident [Arg] Type [TyExpr]
    | Main [TyExpr]
    | StaticString Ident Type Text
    | TypeSyn Ident Type
    | Con Int Ident Type (Maybe [Type])
    deriving (Data, Eq, Ord, Show)


data Arg = Arg Ident Type | EnvArg Type
    deriving (Data, Eq, Ord, Show)


data Operand = ConstantOperand !Constant | LocalReference !Type !Ident


data TyExpr = Typed {typeOf :: Type, expr :: Expr}
    deriving (Data, Eq, Ord, Show)


data Expr
    = Constant Constant
    | Var Binding Ident
    | BinOp TyExpr BinOp TyExpr
    | PrefixOp PrefixOp TyExpr
    | App TyExpr [TyExpr]
    | Let Ident Type (Maybe TyExpr)
    | Ass Ident Type TyExpr
    | Return TyExpr
    | Break
    | If TyExpr [TyExpr] (Maybe [TyExpr])
    | While TyExpr [TyExpr]
    | Closure TyExpr [TyExpr]
    | -- | `ident = ident[integer]`
      ExtractFree Ident Ident Integer
    | StructIndexing TyExpr Integer
    | Match TyExpr [MatchArm] Catch
    | ToStderrExit Ident
    deriving (Data, Eq, Ord, Show)


data Catch = Catch Ident (NonEmpty TyExpr)
    deriving (Data, Eq, Ord, Show)


data MatchArm = MatchArm Pattern (NonEmpty TyExpr)
    deriving (Data, Eq, Ord, Show)


data Pattern = PCon Int [(Ident, Type)]
    deriving (Data, Eq, Ord, Show)


data Binding
    = Free
    | Bound
    | Toplevel
    | Argument
    | Constructor
    | GlobalConstant
    | Lambda
    deriving (Data, Eq, Ord, Show)


data BinOp
    = Mul
    | Div
    | Add
    | Sub
    | Mod
    | Or
    | And
    | Lt
    | Gt
    | Lte
    | Gte
    | Eq
    | Neq
    deriving (Data, Eq, Ord, Show)


data PrefixOp = Not | Neg
    deriving (Data, Eq, Ord, Show)


data Constant
    = IntLit !Integer
    | DoubleLit !Double
    | CharLit !Char
    | BoolLit !Bool
    | UnitLit
    | NullLit
    deriving (Data, Eq, Ord, Show)
