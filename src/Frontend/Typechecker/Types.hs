{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Frontend.Typechecker.Types where

import Control.Lens (makeLenses)
import Data.Data (Data)
import Relude hiding (Any, Type)

import Frontend.Types hiding (Bool, Char, Double, Int, String, Unit)
import Names (Namespace)


data Tc deriving (Data)


data FnType = FnType {tyvars :: [TyVar], retType :: TypeTc, argTypes :: [TypeTc]}
    deriving (Data, Eq, Ord, Show)


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


type TcInfoBound = (SourceInfo, Namespace, TypeTc, Boundedness)


deriving instance Data ProgramTc


deriving instance Data DefTc


deriving instance Data FnTc


deriving instance Data ArgTc


deriving instance Data AdtTc


deriving instance Data ConstructorTc


deriving instance Data ImportTc


deriving instance Data StmtTc


deriving instance Data BlockTc


deriving instance Data ExprTc


deriving instance Data LitTc


deriving instance Data TypeTc


deriving instance Data LamArgTc


deriving instance Data MatchArmTc


deriving instance Data PatternTc


type instance XProgram Tc = Namespace


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


-- FIXME: Make this unique to function calls (and perhaps constructors, or make a separate one) and remove Toplevel from boundedness
data TypeApp = TypeApp {expr :: ExprTc, type_args :: [TypeTc], is_polymorphic :: Bool}
    deriving (Data, Show)


type instance XExpr Tc = TypeApp


type instance XIntLit Tc = NoExtField


type instance XDoubleLit Tc = NoExtField


type instance XStringLit Tc = NoExtField


type instance XCharLit Tc = NoExtField


type instance XBoolLit Tc = NoExtField


type instance XUnitLit Tc = NoExtField


type instance XTyLit Tc = NoExtField


type instance XTyFun Tc = NoExtField


type instance XTyCon Tc = NoExtField


type instance XTypeVar Tc = NoExtField


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


pattern Monotype :: (XType a ~ MetaTy) => Int -> Type a
pattern Monotype n <- Type (Mono (MonoType n))
    where
        Monotype n = Type (Mono $ MonoType n)


data MetaTy = AnyX | Mono MonoType
    deriving (Data, Eq, Ord, Show)


newtype MonoType = MonoType Int
    deriving (Data, Eq, Ord, Show)


data StmtType = StmtType {_stmtType :: TypeTc, _varType :: Type Tc, _stmtInfo :: SourceInfo}
    deriving (Data, Eq, Ord, Show)


data PolyType a = PolyType {tvars :: [TyVar], ty :: Type a}


deriving instance (Forall Show a) => Show (PolyType a)


deriving instance Eq (PolyType Tc)


deriving instance Ord (PolyType Tc)


deriving instance Data (PolyType Tc)


$(makeLenses ''StmtType)
