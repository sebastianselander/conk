{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Frontend.Parser.Types where

import Data.Data (Data)
import Relude hiding (intercalate, replicate, Type)
import Frontend.Types
import Names (Namespace)

data Par
    deriving (Data)

type ExprPar = Expr Par
type ProgramPar = Program Par
type ImportPar = Import Par
type LitPar = Lit Par
type ArgPar = Arg Par
type DefPar = Def Par
type FnPar = Fn Par
type AdtPar = Adt Par
type ConstructorPar = Constructor Par
type TypePar = Type Par
type BlockPar = Block Par
type StmtPar = Stmt Par
type LamArgPar = LamArg Par
type MatchArmPar = MatchArm Par
type PatternPar = Pattern Par

deriving instance Data ExprPar
deriving instance Data ProgramPar
deriving instance Data LitPar
deriving instance Data ArgPar
deriving instance Data DefPar
deriving instance Data ImportPar
deriving instance Data FnPar
deriving instance Data AdtPar
deriving instance Data ConstructorPar
deriving instance Data TypePar
deriving instance Data StmtPar
deriving instance Data BlockPar
deriving instance Data MatchArmPar
deriving instance Data PatternPar

type instance XProgram Par = Namespace

type instance XImport Par = SourceInfo

type instance XArg Par = SourceInfo

type instance XDef Par = DataConCantHappen
type instance XFn Par = SourceInfo

type instance XAdt Par = SourceInfo
type instance XFunCons Par = SourceInfo
type instance XEnumCons Par = SourceInfo
type instance XConstructor Par = DataConCantHappen

type instance XBlock Par = SourceInfo

type instance XStmt Par = DataConCantHappen
type instance XSExp Par = NoExtField

type instance XLit Par = SourceInfo
type instance XVar Par = SourceInfo
type instance XPrefix Par = SourceInfo
type instance XBinOp Par = SourceInfo
type instance XExprStmt Par = SourceInfo
type instance XApp Par = SourceInfo
type instance XRet Par = SourceInfo
type instance XEBlock Par = NoExtField
type instance XBreak Par = SourceInfo
type instance XIf Par = SourceInfo
type instance XWhile Par = SourceInfo
type instance XLet Par = (SourceInfo, Maybe TypePar)
type instance XAss Par = SourceInfo
type instance XExpr Par = DataConCantHappen

type instance XMatchArm Par = SourceInfo
type instance XMatch Par = SourceInfo

type instance XPVar Par = SourceInfo
type instance XPEnumCon Par = SourceInfo
type instance XPFunCon Par = SourceInfo

type instance XIntLit Par = NoExtField
type instance XDoubleLit Par = NoExtField
type instance XStringLit Par = NoExtField
type instance XCharLit Par = NoExtField
type instance XBoolLit Par = NoExtField
type instance XUnitLit Par = NoExtField

type instance XTyLit Par = NoExtField
type instance XTyFun Par = NoExtField
type instance XType Par = DataConCantHappen
type instance XTyCon Par = NoExtField

type instance XLoop Par = SourceInfo
type instance XLam Par = SourceInfo
type instance XLamArg Par = (SourceInfo, Maybe TypePar)

deriving instance Data (LamArg Par)
    
