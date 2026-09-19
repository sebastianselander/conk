{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell #-}

module Frontend.Typechecker.Types where

import Data.Data (Data)
import Relude hiding (Type, Any)
import Frontend.Types hiding (Bool, Unit, Char, String, Double, Int)
import Control.Lens (makeLenses)
import Frontend.Renamer.Types (Boundedness)

data Tc deriving (Data)

data FnType = FnType { retType :: TypeTc, argTypes :: [TypeTc]}
    deriving (Show, Eq, Ord, Data)

type ProgramTc = Program Tc
type DefTc = Def Tc
type ImportTc = Import Tc
type FnTc = Fn Tc
type AdtTc = Adt Tc
type ConstructorTc = Constructor Tc
type ArgTc = Arg Tc
type ExprTc = Expr Tc
type TypeTc = Type Tc
type LitTc = Lit Tc
type StmtTc = Stmt Tc
type BlockTc = Block Tc
type LamArgTc = LamArg Tc
type MatchArmTc = MatchArm Tc
type PatternTc = Pattern Tc

type TcInfo = (SourceInfo, TypeTc)
type TcInfoBound = (SourceInfo, TypeTc, Boundedness)

deriving instance Data StmtTc
deriving instance Data BlockTc
deriving instance Data ExprTc
deriving instance Data LitTc
deriving instance Data TypeTc
deriving instance Data LamArgTc
deriving instance Data MatchArmTc
deriving instance Data PatternTc

type instance XProgram Tc = NoExtField

type instance XArg Tc = NoExtField

type instance XDef Tc = DataConCantHappen
type instance XImport Tc = DataConCantHappen
type instance XImportExplicit Tc = [FnType]
type instance XFn Tc = NoExtField

type instance XAdt Tc = SourceInfo
type instance XConstructor Tc = DataConCantHappen
type instance XEnumCons Tc = TcInfo
type instance XFunCons Tc = TcInfo

type instance XBlock Tc = TcInfo

type instance XStmt Tc = DataConCantHappen
type instance XRet Tc = TcInfo
type instance XEBlock Tc = NoExtField
type instance XBreak Tc = TcInfo
type instance XIf Tc = TcInfo
type instance XWhile Tc = TcInfo
type instance XLet Tc = StmtType
type instance XAss Tc = (StmtType, Boundedness)
type instance XSExp Tc = NoExtField

type instance XMatchArm Tc = SourceInfo
type instance XMatch Tc = TcInfo

type instance XPVar Tc = TcInfo
type instance XPEnumCon Tc = TcInfo
type instance XPFunCon Tc = TcInfo

type instance XLit Tc = TcInfo
type instance XVar Tc = TcInfoBound
type instance XPrefix Tc = TcInfo
type instance XBinOp Tc = TcInfo
type instance XExprStmt Tc = NoExtField
type instance XApp Tc = TcInfo
type instance XExpr Tc = DataConCantHappen

type instance XIntLit Tc = NoExtField
type instance XDoubleLit Tc = NoExtField
type instance XStringLit Tc = NoExtField
type instance XCharLit Tc = NoExtField
type instance XBoolLit Tc = NoExtField
type instance XUnitLit Tc = NoExtField

type instance XTyLit Tc = NoExtField
type instance XTyFun Tc = NoExtField
type instance XTyCon Tc = NoExtField
type instance XType Tc = MetaTy

type instance XLoop Tc = TcInfo
type instance XLam Tc = TcInfo
type instance XLamArg Tc = TypeTc

deriving instance Eq TypeTc
deriving instance Ord TypeTc

pattern Any :: (XType a ~ MetaTy) => Type a
pattern Any <- Type AnyX
    where
        Any = Type AnyX

data MetaTy = AnyX
    deriving (Show, Eq, Ord, Data)

data StmtType = StmtType { _stmtType :: TypeTc, _varType :: TypeTc, _stmtInfo :: SourceInfo}
    deriving (Show, Eq, Ord, Data)
$(makeLenses ''StmtType)

