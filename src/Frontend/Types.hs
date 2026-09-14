{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Frontend.Types where

import Data.Data (Data)
import Data.Kind qualified
import Data.Tuple.Extra (both)
import GHC.Show (show)
import Names
import Relude hiding (Type, concat, intercalate, replicate)
import Relude qualified
import Text.Megaparsec (Pos, mkPos)
import Text.Megaparsec.Pos (unPos)

data NoExtField = NoExtField
    deriving (Show, Eq, Ord, Data, Generic)

data DataConCantHappen
    deriving (Show, Eq, Ord, Data, Generic)

data Span = Span
    { start :: !(Pos, Pos)
    , end :: !(Pos, Pos)
    }
    deriving (Eq, Ord, Data)

emptyInfo :: SourceInfo
emptyInfo = SourceInfo {sourceFile = "", spanInfo = emptySpan}

emptySpan :: Span
emptySpan = Span (mkPos 0, mkPos 0) (mkPos 0, mkPos 0)

instance Show Span where
    show Span {start, end} =
        let (bl, bc) = both (Relude.show . unPos) start
         in Relude.concat [bl, ":", bc]

data SourceInfo = SourceInfo
    { spanInfo :: !Span
    , sourceFile :: !FilePath
    }
    deriving (Eq, Ord, Data)

instance Show SourceInfo where
    show info = info.sourceFile <> ":" <> Relude.show info.spanInfo

-- Program
data Program a = Program !(XProgram a) [Def a]
type family XProgram a

deriving instance (Forall Show a) => Show (Program a)

-- Definition
data Def a
    = DefFn (Fn a)
    | DefAdt (Adt a)
    | DefImport (Import a)
    | DefX !(XDef a)
type family XDef a
deriving instance (Forall Show a) => Show (Def a)

data Fn a = Fn !(XFn a) Ident [Arg a] (Type a) (Block a)
type family XFn a

deriving instance (Forall Show a) => Show (Fn a)

data Import a
    = Import !(XImport a) Namespace [Ident] -- import foo (bar, baz)
    | ImportQualified !(XImport a) Namespace -- import foo.bar.baz
    | ImportAs !(XImport a) Namespace Ident -- import foo.bar as baz
    -- Add ImportX and move ImportAs and Import to here, renamer makes them both ImportQualified
type family XImport a
deriving instance (Forall Show a) => Show (Import a)

data Adt a = Adt !(XAdt a) Ident [Constructor a]
type family XAdt a
deriving instance (Forall Show a) => Show (Adt a)

data Constructor a
    = EnumCons (XEnumCons a) Ident
    | FunCons (XFunCons a) Ident [Type a]
    | ConstructorX !(XConstructor a)

type family XConstructor a
type family XEnumCons a
type family XFunCons a
deriving instance (Forall Show a) => Show (Constructor a)

-- Argument
data Arg a = Arg !(XArg a) Ident (Type a)
type family XArg a

deriving instance (Forall Show a) => Show (Arg a)

-- Type
data Type a
    = TyLit !(XTyLit a) TyLit
    | TyFun !(XTyFun a) [Type a] (Type a)
    | Type !(XType a)
    | TyCon !(XTyCon a) Ident
type family XTyLit a
type family XTyFun a
type family XType a
type family XTyCon a

coerceType ::
    (XTyLit t1 ~ XTyLit t2, XTyFun t1 ~ XTyFun t2, XType t1 ~ XType t2, XTyCon t1 ~ XTyCon t2) =>
    Type t1 ->
    Type t2
coerceType ty = case ty of
    TyLit a b -> TyLit a b
    TyFun a b c -> TyFun a (fmap coerceType b) (coerceType c)
    TyCon a b -> TyCon a b
    Type a -> Type a

data TyLit = Unit | String | Int | Double | Char | Bool
    deriving (Show, Eq, Ord, Enum, Data)

deriving instance (Forall Show a) => Show (Type a)

data Block a = Block !(XBlock a) [Stmt a] (Maybe (Expr a))
type family XBlock a

deriving instance (Forall Show a) => Show (Block a)

-- Statement
data Stmt a
    = SExpr !(XSExp a) (Expr a)
    | Stmt !(XStmt a)
type family XSExp a
type family XStmt a

data AssignOp
    = AddAssign
    | SubAssign
    | MulAssign
    | DivAssign
    | ModAssign
    | Assign
    deriving (Show, Eq, Ord, Data)

deriving instance (Forall Show a) => Show (Stmt a)

-- Expression
data Expr a
    = Lit !(XLit a) (Lit a)
    | Var !(XVar a) Ident
    | BinOp !(XBinOp a) (Expr a) BinOp (Expr a)
    | Prefix !(XPrefix a) PrefixOp (Expr a)
    | App !(XApp a) (Expr a) [Expr a]
    | Let !(XLet a) Ident (Expr a)
    | Ass !(XAss a) Ident AssignOp (Expr a)
    | Ret !(XRet a) (Maybe (Expr a))
    | EBlock !(XEBlock a) (Block a)
    | Break !(XBreak a) (Maybe (Expr a))
    | If !(XIf a) (Expr a) (Block a) (Maybe (Block a))
    | While !(XWhile a) (Expr a) (Block a)
    | Loop !(XLoop a) (Block a)
    | Lam !(XLam a) [LamArg a] (Expr a)
    | Match !(XMatch a) (Expr a) [MatchArm a]
    | Expr !(XExpr a)

deriving instance (Forall Show a) => Show (Expr a)

type family XExprStmt a
type family XLit a
type family XVar a
type family XPrefix a
type family XBinOp a
type family XApp a
type family XAss a
type family XLet a
type family XRet a
type family XEBlock a
type family XBreak a
type family XIf a
type family XWhile a
type family XExpr a
type family XLoop a
type family XLam a
type family XMatch a

data MatchArm a = MatchArm !(XMatchArm a) (Pattern a) (Expr a)

deriving instance (Forall Show a) => Show (MatchArm a)

type family XMatchArm a

data Pattern a
    = PVar !(XPVar a) Ident
    | PEnumCon !(XPEnumCon a) Ident
    | PFunCon !(XPFunCon a) Ident [Pattern a]

deriving instance (Forall Show a) => Show (Pattern a)

type family XPVar a
type family XPEnumCon a
type family XPFunCon a

data LamArg a = LamArg !(XLamArg a) Ident
type family XLamArg a

deriving instance (Forall Show a) => Show (LamArg a)

data PrefixOp = Not | Neg
    deriving (Show, Eq, Ord, Data)

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
    deriving (Show, Eq, Ord, Data)

-- Literal
data Lit a
    = IntLit !(XIntLit a) Integer
    | DoubleLit !(XDoubleLit a) Double
    | StringLit !(XStringLit a) Text
    | CharLit !(XCharLit a) Char
    | BoolLit !(XBoolLit a) Bool
    | UnitLit !(XUnitLit a)
type family XIntLit a
type family XDoubleLit a
type family XStringLit a
type family XCharLit a
type family XBoolLit a
type family XUnitLit a

deriving instance (Forall Show a) => Show (Lit a)

type Forall (c :: Data.Kind.Type -> Constraint) a =
    ( c (XApp a)
    , c (XArg a)
    , c (XPrefix a)
    , c (XBinOp a)
    , c (XBlock a)
    , c (XBoolLit a)
    , c (XBreak a)
    , c (XCharLit a)
    , c (XUnitLit a)
    , c (XDef a)
    , c (XDoubleLit a)
    , c (XExprStmt a)
    , c (XIf a)
    , c (XIntLit a)
    , c (XLet a)
    , c (XAss a)
    , c (XLit a)
    , c (XProgram a)
    , c (XRet a)
    , c (XSExp a)
    , c (XStmt a)
    , c (XStringLit a)
    , c (XTyLit a)
    , c (XTyCon a)
    , c (XTyFun a)
    , c (XVar a)
    , c (XWhile a)
    , c (XEBlock a)
    , c (XExpr a)
    , c (XType a)
    , c (XLoop a)
    , c (XLam a)
    , c (XLamArg a)
    , c (XFn a)
    , c (XAdt a)
    , c (XImport a)
    , c (XConstructor a)
    , c (XEnumCons a)
    , c (XFunCons a)
    , c (XMatchArm a)
    , c (XMatch a)
    , c (XPVar a)
    , c (XPEnumCon a)
    , c (XPFunCon a)
    )
