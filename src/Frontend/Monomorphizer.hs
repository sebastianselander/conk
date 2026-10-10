{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}
module Frontend.Monomorphizer where

import Control.Monad (foldM)
import Data.Either.Extra (fromEither)
import Data.Text.Lazy (unpack)
import Relude hiding (Type)
import Text.Pretty.Simple (pShow)

import Data.Map qualified as Map
import Prettyprinter qualified as Pretty

import Frontend.MonomorphizerCollector (Item (..))
import Frontend.Substitution (Substitution (Subst), apply)
import Frontend.Typechecker.Polytype (instantiate_with, replaceTyVars)
import Frontend.Typechecker.Types (Tc)
import Frontend.Types
    ( Arg (Arg),
      Def (DefFn),
      Fn (Fn),
      NoExtField (NoExtField),
      Program (Program),
      Type (TypeVar),
      emptyTyParamList,
      toList,
    )
import Impossible (__IMPOSSIBLE__)
import Names (Ident (Ident))

import Names qualified


monomorphize :: Program Tc -> [Item] -> Program Tc
monomorphize prg items = foldl' insert_mono_fn prg (deduplicate items)


-- We can optimize this by grouping items by the function they are monomorphizing.
insert_mono_fn :: Program Tc -> Item -> Program Tc
insert_mono_fn (Program namespace defs) item = Program namespace (DefFn mono_fn : defs)
  where
    Fn NoExtField original_name typaramlist args original_type body =
        fromMaybe __IMPOSSIBLE__ (find_fn item.name defs)
    sub =
        Subst
            $ Map.fromList (zip (fmap (TypeVar NoExtField) (Frontend.Types.toList typaramlist)) item.type_args)
    mono_fn =
        Fn
            NoExtField
            ( Names.prepend "\""
                $ Names.append
                    ( show
                        ( Pretty.angles
                            ( Pretty.concatWith
                                (Pretty.surround Pretty.comma)
                                (fmap Pretty.pretty item.type_args)
                            )
                            <> "\""
                        )
                    )
                    original_name
            )
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
