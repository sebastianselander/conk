{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}
module Frontend.MonomorphizerCollector where

import Relude hiding (Type)

import Data.Set qualified as Set

import Frontend.Typechecker.Types (Tc, TypeApp (TypeApp))
import Frontend.Types (Expr (..), Program, SourceInfo, Type)
import Names (Ident)
import Utils (listify')


newtype Collection = Collection {items :: Set Item}
    deriving (Eq, Ord, Show)


data Item = Item {name :: Ident, ty :: Type Tc, type_args :: [Type Tc], loc :: SourceInfo}
    deriving (Eq, Ord, Show)


collect :: Program Tc -> Collection
collect = Collection . Set.fromList . listify' find_type_app


find_type_app :: Expr Tc -> Maybe Item
find_type_app expr = case expr of
    -- NOTE: Type application on a variable is guaranteed by the typechecker to be a toplevel function
    Expr (TypeApp (Var (loc, ty, _) name) type_args) -> Just (Item name ty type_args loc)
    _ -> Nothing
