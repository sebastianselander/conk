{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}

module Frontend.Typechecker.Polytype where

import Control.Exception (assert)
import Data.Text (pack)
import Relude hiding (Any, Type, toList)

import Data.Map qualified as Map
import Data.Set qualified as Set

import Frontend.Substitution (Substitution (..), apply)
import Frontend.Typechecker.Types
    ( MetaTy (..),
      MonoType,
      PolyType (PolyType),
      Tc,
      pattern Monotype,
    )
import Frontend.Types (NoExtField (NoExtField), TyVar (TyVar), Type (..))
import Names (Ident (Ident))


instantiate_with :: Map TyVar MonoType -> PolyType Tc -> Type Tc
instantiate_with tbl (PolyType tyvars ty) =
    assert (and [Map.member tyvar tbl | tyvar <- tyvars])
        $ replaceTyVars (Map.map (Type . Mono) tbl) ty


instantiate :: Int -> PolyType Tc -> (Type Tc, Int)
instantiate n (PolyType typevars ty) =
    let tvars_to_replace = Map.fromList $ zipWith (\ty n -> (ty, Monotype n)) typevars [n ..]
     in (replaceTyVars tvars_to_replace ty, n + Map.size tvars_to_replace)


replaceTyVars :: Map TyVar (Type Tc) -> Type Tc -> Type Tc
replaceTyVars tbl = \case
    TyLit NoExtField lit -> TyLit NoExtField lit
    TyFun tyParamList args ret -> TyFun tyParamList (fmap (replaceTyVars tbl) args) (replaceTyVars tbl ret)
    TyCon NoExtField name -> TyCon NoExtField name
    t@(TypeVar NoExtField tyvar) -> fromMaybe t (Map.lookup tyvar tbl)
    Type meta -> Type meta


occurs :: MonoType -> Type Tc -> Bool
occurs mono ty = case ty of
    TyLit NoExtField _ -> False
    TyCon NoExtField _ -> False
    TypeVar NoExtField _ -> False
    TyFun NoExtField args ret -> any (occurs mono) args || occurs mono ret
    Type (Mono m) -> mono == m
    Type AnyX -> False


generalize :: Type Tc -> (PolyType Tc, Substitution Tc)
generalize ty =
    let (monos, tvars) = find_all_monotypes ty
        generalized_tvars = TyVar <$> tvar_names tvars
        table = zip (Set.toList monos) generalized_tvars
        subst = Subst $ Map.fromList [(Type (Mono k), TypeVar NoExtField v) | (k, v) <- table]
     in (PolyType (fmap snd table) $ apply subst ty, subst)
  where
    find_all_monotypes :: Type Tc -> (Set MonoType, Set Ident)
    find_all_monotypes ty = case ty of
        TyFun NoExtField args ret ->
            let (monos, tvars) = foldr f (mempty, mempty) args
                (mono, tvar) = find_all_monotypes ret
             in (mono <> monos, tvar <> tvars)
        Type (Mono m) -> (Set.singleton m, mempty)
        TyLit NoExtField _ -> (mempty, mempty)
        TyCon NoExtField _ -> (mempty, mempty)
        TypeVar NoExtField _ -> (mempty, mempty)
        Type AnyX -> (mempty, mempty)
      where
        f t (monos, tvars) =
            let (monos2, tvars2) = find_all_monotypes t
             in (monos <> monos2, tvars <> tvars2)


tvar_names :: Set Ident -> [Ident]
tvar_names exclude =
    filter (`Set.notMember` exclude)
        $ fmap (Ident . pack)
        $ [1 ..]
        >>= flip replicateM ['A' .. 'Z']


{- ===== Testing ====== -}

tyvar :: Text -> Type Tc
tyvar name = TypeVar NoExtField (TyVar $ Ident name)


ty :: PolyType Tc
ty = PolyType [TyVar "a"] $ TyFun NoExtField [tyvar "b", tyvar "a"] (tyvar "a")
