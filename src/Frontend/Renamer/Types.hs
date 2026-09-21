{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Frontend.Renamer.Types where

import Data.Data (Data)
import Frontend.Parser.Types
import Frontend.Types
import Names (Namespace)
import Prettyprinter
import Relude hiding (Type)

data Boundedness = Free | Bound | Toplevel | Constructor | Imported | Builtin
    deriving (Show, Eq, Ord, Data)

instance Pretty Boundedness where
    pretty = show

data Rn
    deriving (Data)

type ProgramRn = Program Rn
type DefRn = Def Rn
type ImportRn = Import Rn
type FnRn = Fn Rn
type AdtRn = Adt Rn
type ConstructorRn = Constructor Rn
type ArgRn = Arg Rn
type ExprRn = Expr Rn
type TypeRn = Type Rn
type LitRn = Lit Rn
type StmtRn = Stmt Rn
type BlockRn = Block Rn
type LamArgRn = LamArg Rn
type MatchArmRn = MatchArm Rn
type PatternRn = Pattern Rn

deriving instance Data ExprRn
deriving instance Data ProgramRn
deriving instance Data LitRn
deriving instance Data ArgRn
deriving instance Data DefRn
deriving instance Data ImportRn
deriving instance Data FnRn
deriving instance Data AdtRn
deriving instance Data ConstructorRn
deriving instance Data TypeRn
deriving instance Data BlockRn
deriving instance Data StmtRn
deriving instance Data LamArgRn
deriving instance Data MatchArmRn
deriving instance Data PatternRn

type instance XProgram Rn = XProgram Par

type instance XArg Rn = (SourceInfo, Namespace)

type instance XDef Rn = XDef Par
type instance XImport Rn = DataConCantHappen
type instance XImportExplicit Rn = SourceInfo
type instance XFn Rn = XFn Par

type instance XAdt Rn = XAdt Par
type instance XConstructor Rn = XConstructor Par
type instance XEnumCons Rn = XEnumCons Par
type instance XFunCons Rn = XFunCons Par

type instance XBlock Rn = XBlock Par

type instance XRet Rn = XRet Par
type instance XBreak Rn = XBreak Par
type instance XEBlock Rn = NoExtField
type instance XIf Rn = XIf Par
type instance XWhile Rn = XWhile Par
type instance XLet Rn = (SourceInfo, Maybe TypeRn)
type instance XAss Rn = (SourceInfo, Boundedness, Namespace)
type instance XSExp Rn = XSExp Par
type instance XStmt Rn = DataConCantHappen

type instance XMatchArm Rn = SourceInfo
type instance XMatch Rn = SourceInfo

type instance XPVar Rn = SourceInfo
type instance XPEnumCon Rn = (SourceInfo, Namespace)
type instance XPFunCon Rn = (SourceInfo, Namespace)

type instance XLit Rn = XLit Par
type instance XVar Rn = (SourceInfo, Namespace, Boundedness)
type instance XBinOp Rn = XBinOp Par
type instance XPrefix Rn = XBinOp Par
type instance XExprStmt Rn = XExprStmt Par
type instance XApp Rn = XApp Par
type instance XExpr Rn = DataConCantHappen

type instance XIntLit Rn = NoExtField
type instance XDoubleLit Rn = NoExtField
type instance XStringLit Rn = NoExtField
type instance XCharLit Rn = NoExtField
type instance XBoolLit Rn = NoExtField
type instance XUnitLit Rn = NoExtField

type instance XTyLit Rn = NoExtField
type instance XTyFun Rn = NoExtField
type instance XType Rn = DataConCantHappen
type instance XTyCon Rn = NoExtField
type instance XTypeVar Rn = NoExtField

type instance XLoop Rn = SourceInfo
type instance XLam Rn = XLam Par
type instance XLamArg Rn = (SourceInfo, Maybe TypeRn, Namespace)
