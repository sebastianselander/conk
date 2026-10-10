{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}

module Frontend.Monomorphizer where

import Control.Monad (foldM)
import Data.Either.Extra (fromEither)
import Data.Generics (everywhere, mkT)
import Relude hiding (Type)

import Data.Map qualified as Map
import Prettyprinter qualified as Pretty

import Frontend.MonomorphizerCollector (Item (..))
import Frontend.Substitution (Substitution (Subst), apply)
import Frontend.Typechecker.Types (Tc, TypeApp (TypeApp))
import Frontend.Types
    ( Arg (Arg),
      Def (DefFn),
      Expr (Expr, Var),
      Fn (Fn),
      NoExtField (NoExtField),
      Program (Program),
      Type (TypeVar),
      emptyTyParamList,
      toList,
    )
import Impossible (__IMPOSSIBLE__)
import Names (Ident)

import Names qualified


type RenameTable = Map (Ident, [Type Tc]) Ident


monomorphize :: Program Tc -> [Item] -> (Program Tc, RenameTable)
monomorphize prg items = runState (foldM insert_mono_fn prg (deduplicate items)) Map.empty


-- FIXME(sebsel): Apply rename to imports too
apply_rename :: RenameTable -> Program Tc -> Program Tc
apply_rename tbl = everywhere (mkT (rename tbl))


rename :: RenameTable -> Expr Tc -> Expr Tc
rename tbl expr = case expr of
    Expr (TypeApp (Var (loc, namespace, ty, bind) name) type_args) ->
        Expr
            ( TypeApp
                (Var (loc, namespace, ty, bind) (fromMaybe name (Map.lookup (name, type_args) tbl)))
                type_args
            )
    _ -> expr


make_mono_name ::
    -- | Type arguments
    [Type Tc] ->
    -- | Original name
    Ident ->
    -- \| New name
    Ident
make_mono_name type_args =
    Names.prepend "\""
        . Names.append
            ( show
                ( Pretty.angles
                    ( Pretty.concatWith
                        (Pretty.surround Pretty.comma)
                        (fmap Pretty.pretty type_args)
                    )
                    <> "\""
                )
            )


-- We can optimize this by grouping items by the function they are monomorphizing.
insert_mono_fn ::
    (MonadState RenameTable m) => Program Tc -> Item -> m (Program Tc)
insert_mono_fn (Program namespace defs) item = do
    fn <- mono_fn
    pure (Program namespace (DefFn fn : defs))
  where
    Fn NoExtField original_name typaramlist args original_type body =
        fromMaybe __IMPOSSIBLE__ (find_fn item.name defs)
    sub =
        Subst
            $ Map.fromList
                ( zip
                    (fmap (TypeVar NoExtField) (Frontend.Types.toList typaramlist))
                    item.type_args
                )
    name = make_mono_name item.type_args original_name
    mono_fn = do
        modify (Map.insert (original_name, item.type_args) name)
        pure
            $ Fn
                NoExtField
                name
                emptyTyParamList
                (fmap (\(Arg _ name ty) -> Arg NoExtField name (apply sub ty)) args)
                (apply sub original_type)
                (apply sub body)


find_fn ::
    -- | Function we're looking for
    Ident ->
    -- | All defs in the program
    [Def Tc] ->
    -- | Pair of function X we were looking for and all defs \\ function X
    Maybe (Fn Tc)
find_fn name = foldr f Nothing
  where
    f :: Def Tc -> Maybe (Fn Tc) -> Maybe (Fn Tc)
    f _ (Just fn) = Just fn
    f def Nothing = case def of
        DefFn fn@(Fn _ fnname _ _ _ _) | name == fnname -> Just fn
        _ -> Nothing


deduplicate :: [Item] -> [Item]
deduplicate = Map.elems . fromEither . foldM f Map.empty
  where
    f acc item@(Item {name, namespace = _, type_args, ty = _, loc = _}) =
        case Map.lookup (name, type_args) acc of
            Just _ -> Left acc
            Nothing -> Right (Map.insert (name, type_args) item acc)
