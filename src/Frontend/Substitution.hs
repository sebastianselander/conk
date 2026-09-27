{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

module Frontend.Substitution where

import Data.Map qualified as Map
import Frontend.Typechecker.Types (Tc)
import Frontend.Types (Forall, NoExtField (NoExtField), TyVar (..), Type (..), emptyInfo, (~~))
import Names (Ident (..))
import Relude hiding (Type, empty)

newtype Substitution a = Subst (Map TyVar (Type a))

deriving instance (Forall Show a) => Show (Substitution a)

empty :: Substitution a
empty = Subst mempty

singleton :: TyVar -> Type a -> Substitution a
singleton tyvar ty = Subst (Map.singleton tyvar ty)

lookup :: TyVar -> Substitution a -> Maybe (Type a)
lookup tyvar (Subst sub) = Map.lookup tyvar sub

insert :: TyVar -> Type a -> Substitution a -> Maybe (Substitution a)
insert tyvar ty (Subst sub)
    | Just found <- Map.lookup tyvar sub = if found ~~ ty then Just (Subst sub) else Nothing
    | otherwise = Just (Subst (Map.insert tyvar ty sub))

{-| compose two substitutions, fails if two non-compatible types occur for the same type variable
If we compose `[a |-> b]` and `[b |-> int]` then we get `[a |-> int, b |-> int]`
-}
compose :: forall a. Substitution a -> Substitution a -> Maybe (Substitution a)
compose sub1@(Subst m1) (Subst m2) = do
    let sub2 = Map.mapWithKey f m2
     in if all isJust sub2 then Just (Subst (Map.union m1 (Map.mapMaybe id sub2))) else Nothing
  where
    f :: TyVar -> Type a -> Maybe (Type a)
    f tyvar ty = case lookup tyvar sub1 of
        Nothing -> Just (substitute sub1 ty)
        Just found
            | found ~~ ty -> Just found
            | otherwise -> Nothing

substitute :: Substitution a -> Type a -> Type a
substitute sub ty = case ty of
    TyFun loc args ret -> TyFun loc (fmap (substitute sub) args) (substitute sub ret)
    TypeVar _ tyvar -> fromMaybe ty (lookup tyvar sub)
    TyLit _ _ -> ty
    TyCon _ _ -> ty
    Type _ -> ty

test1 :: IO ()
test1 = do
    let s1 :: Substitution Tc
        s1 = empty
    let s2 :: Substitution Tc
        s2 = empty
    let Just s1' = insert (TyVar (Ident "a")) (TyCon NoExtField (Ident "Bool")) s1
    let Just s2' = insert (TyVar (Ident "b")) (TyCon NoExtField (Ident "Int")) s2
    let s = compose s1' s2'
    print s
