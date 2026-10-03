{-# LANGUAGE UndecidableInstances #-}

module Frontend.Typechecker.Polytype where

import Data.Map qualified as Map
import Frontend.Typechecker.Types (MetaTy (..), Tc)
import Frontend.Types (Forall, NoExtField (NoExtField), TyVar (TyVar), Type (..))
import Relude hiding (Type, toList)

data PolyType a = PolyType [TyVar] (Type a)

deriving instance (Forall Show a) => Show (PolyType a)

instantiate :: PolyType Tc -> Type Tc
instantiate (PolyType typevars ty) =
    let replaceTyVars :: Map TyVar (Type Tc) -> Type Tc -> Type Tc
        replaceTyVars tbl = \case
            TyLit NoExtField lit -> TyLit NoExtField lit
            TyFun tyParamList args ret -> TyFun tyParamList (fmap (replaceTyVars tbl) args) (replaceTyVars tbl ret)
            TyCon NoExtField name -> TyCon NoExtField name
            t@(TypeVar NoExtField tyvar) -> fromMaybe t (Map.lookup tyvar tbl)
            Type meta -> Type meta
        tvars_to_replace = Map.fromList $ zipWith (\ty n -> (ty, Type (MonoType n))) typevars [1 ..]
     in replaceTyVars tvars_to_replace ty

occurs :: Int -> Type Tc -> Bool
occurs n ty = case ty of
    TyLit NoExtField _ -> False
    TyCon NoExtField _ -> False
    TypeVar NoExtField _ -> False
    TyFun NoExtField args ret -> any (occurs n) args || occurs n ret
    Type (MonoType m) -> n == m
    Type AnyX -> False

test :: Type Tc
test =
    instantiate
        ( PolyType
            [TyVar "a", TyVar "b"]
            (TyFun NoExtField [TypeVar NoExtField (TyVar "a")] (TypeVar NoExtField (TyVar "a")))
        )
