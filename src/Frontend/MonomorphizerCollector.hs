{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}
module Frontend.MonomorphizerCollector where

import Prettyprinter ((<+>))
import Relude hiding (Type)

import Data.Set qualified as Set
import Prettyprinter qualified as Pretty

import Frontend.Typechecker.Pretty ()
import Frontend.Typechecker.Types (Tc, TypeApp (TypeApp))
import Frontend.Types (Expr (..), Program (..), SourceInfo, Type)
import Names (Ident, Namespace)
import Utils (listify')


data Collection = Collection {namespace :: Namespace, items :: Set Item}
    deriving (Eq, Ord, Show)


data Item = Item
    {name :: Ident, namespace :: Namespace, ty :: Type Tc, type_args :: [Type Tc], loc :: SourceInfo}
    deriving (Eq, Ord, Show)


prettyItem :: Item -> Text
prettyItem item =
    show
        $ Pretty.pretty item.namespace
        <> "::"
        <> Pretty.pretty item.name
        <> Pretty.angles
            ( Pretty.concatWith (Pretty.surround (Pretty.comma <> Pretty.space))
                $ fmap Pretty.pretty item.type_args
            )
        <> ":"
        <+> Pretty.pretty item.ty
        <+> "used at"
        <+> show item.loc


collect :: Program Tc -> Collection
collect prg@(Program namespace _) = Collection namespace $ Set.fromList $ listify' find_type_app prg


find_type_app :: Expr Tc -> Maybe Item
find_type_app expr = case expr of
    -- NOTE: Type application on a variable is guaranteed by the typechecker to be a toplevel function
    Expr (TypeApp (Var (loc, namespace, ty, _) name) type_args True) ->
        Just (Item name namespace ty type_args loc)
    _ -> Nothing
