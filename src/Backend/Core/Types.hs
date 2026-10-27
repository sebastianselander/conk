module Backend.Core.Types where

import Data.Data (Data)
import Relude hiding (Type)

import Backend.Types (Type (..))
import Names (ExplicitName, Namespace)
import Origin (Origin (..))

newtype Program = Program (NonEmpty Module)
    deriving (Data, Eq, Ord, Show)

data Module = Module Namespace [Def]
    deriving (Data, Eq, Ord, Show)

data Def
    = Decl Namespace Type ExplicitName [Type]
    | Fn !Origin ExplicitName [Arg] Type [TyExpr]
    | Main Namespace [TyExpr]
    | StaticString ExplicitName Type Text
    | TypeSyn ExplicitName Type
    | Con Int ExplicitName Type [Type]
    deriving (Data, Eq, Ord, Show)

data Arg = Arg ExplicitName Type | EnvArg Namespace Type
    deriving (Data, Eq, Ord, Show)

data Operand = ConstantOperand !Literal | LocalReference !Type !ExplicitName

data TyExpr = Typed {typeOf :: Type, expr :: Expr}
    deriving (Data, Eq, Ord, Show)

data Expr
    = Constant Literal
    | Var Binding ExplicitName
    | BinOp TyExpr BinOp TyExpr
    | PrefixOp PrefixOp TyExpr
    | App TyExpr [TyExpr]
    | Let ExplicitName Type (Maybe TyExpr)
    | Ass ExplicitName Type TyExpr
    | DerefAss ExplicitName Type TyExpr
    | Return TyExpr
    | Break
    | If TyExpr [TyExpr] (Maybe [TyExpr])
    | While TyExpr [TyExpr]
    | Closure TyExpr [TyExpr]
    | -- | `ident = ident[integer]`
      ExtractFree ExplicitName ExplicitName Integer
    | StructIndexing TyExpr Integer
    | Match TyExpr [MatchArm] Catch
    | ToStderrExit ExplicitName
    deriving (Data, Eq, Ord, Show)

data Catch = Catch ExplicitName (NonEmpty TyExpr)
    deriving (Data, Eq, Ord, Show)

data MatchArm = MatchArm Pattern (NonEmpty TyExpr)
    deriving (Data, Eq, Ord, Show)

data Pattern = PCon Int [(ExplicitName, Type)]
    deriving (Data, Eq, Ord, Show)

data Binding
    = Free
    | Bound
    | Function
    | Argument
    | Constructor
    | GlobalConstant
    | Lambda
    deriving (Data, Eq, Ord, Show)

data BinOp
    = MulOp
    | DivOp
    | AddOp
    | SubOp
    | ModOp
    | OrOp
    | AndOp
    | LtOp
    | GtOp
    | LteOp
    | GteOp
    | EqOp
    | NeqOp
    deriving (Data, Eq, Ord, Show)

data PrefixOp = Not | Neg | Ref | Deref
    deriving (Data, Eq, Ord, Show)

data Literal
    = IntLit !Integer
    | DoubleLit !Double
    | CharLit !Char
    | BoolLit !Bool
    | UnitLit
    | NullLit
    deriving (Data, Eq, Ord, Show)
