{-# LANGUAGE UndecidableInstances #-}

module Frontend.Substitution where

import Control.Lens (over)
import Data.Text (pack)
import Relude hiding (Type, empty)

import Data.Map qualified as Map
import Prettyprinter qualified as Pretty

import Frontend.Typechecker.Pretty ()
import Frontend.Typechecker.Types
    ( BlockTc,
      MetaTy (AnyX, Mono),
      PolyType (..),
      StmtTc,
      StmtType,
      Tc,
      TypeApp (TypeApp),
      stmtType,
      varType,
    )
import Frontend.Types
    ( Block (..),
      Expr (..),
      Forall,
      LamArg (..),
      MatchArm (..),
      NoExtField (NoExtField),
      Pattern (..),
      Stmt (SExpr),
      Type (..),
      XType,
      (~~),
    )


newtype Substitution a = Subst (Map (Type a) (Type a))


pretty :: Substitution Tc -> Text
pretty (Subst m) =
    pack
        $ "[ "
        <> intercalate
            ", "
            [show (Pretty.pretty k) <> " == " <> show (Pretty.pretty v) | (k, v) <- Map.toList m]
        <> " ]"


deriving instance (Forall Show a) => Show (Substitution a)


deriving instance Eq (Substitution Tc)


empty :: Substitution a
empty = Subst Map.empty


singleton :: Type a -> Type a -> Substitution a
singleton tyvar ty = Subst (Map.singleton tyvar ty)


lookup :: (Ord (Type a)) => Type a -> Substitution a -> Maybe (Type a)
lookup tyvar (Subst sub) = Map.lookup tyvar sub


insert ::
    (Eq (XType a), Ord (Type a)) => Type a -> Type a -> Substitution a -> Maybe (Substitution a)
insert tyvar ty (Subst sub)
    | Just found <- Map.lookup tyvar sub = if found ~~ ty then Just (Subst sub) else Nothing
    | otherwise = Just (Subst (Map.insert tyvar ty sub))


{-
apply (compose a b) t == apply a (apply b) t

We want to apply the oldest substition first because the ones created after
might depend on some result from the first
-}
compose :: Substitution Tc -> Substitution Tc -> Substitution Tc
compose sub1@(Subst m1) (Subst m2) = Subst $ Map.map (apply sub1) m2 `Map.union` m1


substitute :: Substitution Tc -> Type Tc -> Type Tc
substitute sub ty = case ty of
    Type (Mono _) -> fromMaybe ty (lookup ty sub)
    TyFun loc args ret -> TyFun loc (fmap (substitute sub) args) (substitute sub ret)
    Type AnyX -> ty
    TypeVar _ _ -> ty
    TyLit _ _ -> ty
    TyCon _ _ -> ty


class Substitute t where
    apply :: Substitution Tc -> t -> t


instance Substitute (Type Tc) where
    apply sub ty = case ty of
        Type (Mono _) -> fromMaybe ty (lookup ty sub)
        TyFun NoExtField args ret -> fromMaybe (TyFun NoExtField (fmap (apply sub) args) (apply sub ret)) (lookup ty sub)
        Type AnyX -> fromMaybe ty (lookup ty sub)
        TypeVar NoExtField _ -> fromMaybe ty (lookup ty sub)
        TyLit NoExtField _ -> fromMaybe ty (lookup ty sub)
        TyCon NoExtField _ -> fromMaybe ty (lookup ty sub)


instance Substitute (Expr Tc) where
    apply sub expr = case expr of
        Lit _ _ -> expr
        Var (loc, namespace, ty, binding) name -> Var (loc, namespace, apply sub ty, binding) name
        BinOp (loc, ty) l op r -> BinOp (loc, apply sub ty) (apply sub l) op (apply sub r)
        Prefix (loc, ty) op expr -> Prefix (loc, apply sub ty) op (apply sub expr)
        App (loc, ty) l rs -> App (loc, apply sub ty) (apply sub l) (fmap (apply sub) rs)
        Let stmtType name expr -> Let (apply sub stmtType) name (apply sub expr)
        Ass (loc, binding) name op expr -> Ass (loc, binding) name op (apply sub expr)
        Ret (loc, ty) mexpr -> Ret (loc, apply sub ty) (fmap (apply sub) mexpr)
        EBlock NoExtField block -> EBlock NoExtField $ apply sub block
        Break (loc, ty) mexpr -> Break (loc, apply sub ty) (fmap (apply sub) mexpr)
        If (loc, ty) scrutinee trueBlock falseBlock -> If (loc, apply sub ty) (apply sub scrutinee) (apply sub trueBlock) (fmap (apply sub) falseBlock)
        While (loc, ty) expr block -> While (loc, apply sub ty) (apply sub expr) (apply sub block)
        Loop (loc, ty) block -> Loop (loc, apply sub ty) (apply sub block)
        Lam (loc, ty) args body -> Lam (loc, apply sub ty) (fmap (apply sub) args) (apply sub body)
        Match (loc, ty) scrutinee matchArms -> Match (loc, apply sub ty) (apply sub scrutinee) (fmap (apply sub) matchArms)
        Expr (TypeApp expr type_args) -> Expr (TypeApp (apply sub expr) (fmap (apply sub) type_args))


instance Substitute (LamArg Tc) where
    apply sub (LamArg ty name) = LamArg (apply sub ty) name


instance Substitute (MatchArm Tc) where
    apply sub (MatchArm loc pat expr) = MatchArm loc (apply sub pat) (apply sub expr)


instance Substitute (Pattern Tc) where
    apply sub pat = case pat of
        PVar (loc, ty) name -> PVar (loc, apply sub ty) name
        PEnumCon (loc, ty) name -> PEnumCon (loc, apply sub ty) name
        PFunCon (loc, ty) name pats -> PFunCon (loc, apply sub ty) name (fmap (apply sub) pats)


instance Substitute BlockTc where
    apply sub (Block (loc, ty) stmts mret) = Block (loc, apply sub ty) (fmap (apply sub) stmts) (fmap (apply sub) mret)


instance Substitute StmtTc where
    apply sub (SExpr NoExtField expr) = SExpr NoExtField (apply sub expr)


instance Substitute StmtType where
    apply sub d = over stmtType (apply sub) $ over varType (apply sub) d


instance Substitute (PolyType Tc) where
    apply sub (PolyType tvars ty) = PolyType tvars (apply sub ty)
