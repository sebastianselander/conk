{-# LANGUAGE OverloadedStrings #-}

module Names
    ( Ident (..),
      Names,
      Namespace( ..),
      mkNames,
      getOriginalName,
      mkNamespace,
      existName,
      getText,
      insertName,
      getOriginalName',
      renameBack,
      combine,
      intercalate,
    ) where

import Data.Data (Data)
import Data.Map qualified as Map
import Generics.SYB (everywhere, mkT)
import Prettyprinter (Pretty (..), concatWith, dot, surround)
import Relude hiding (intercalate)
import System.FilePath (splitDirectories)
import Data.Text (pack)

newtype Names = Names {unNames :: Map Ident Ident}
    deriving (Show, Data)

-- | Prefers items in the first `Names`
combine :: Names -> Names -> Names
combine (Names names1) (Names names2) = Names (Map.union names1 names2)

mkNames :: Map Ident Ident -> Names
mkNames = Names

mkNamespace :: String -> Namespace
mkNamespace name = Namespace $ fmap pack (fromList (splitDirectories name))

-- Namespace, e.g: `foo.bar.baz`, here `foo.bar` is the namespace and `baz` is an Ident
newtype Namespace = Namespace (NonEmpty Text)
    deriving (Show, Eq, Ord, Data, Semigroup)

-- Identifier: `foo`
newtype Ident = Ident Text
    deriving (Show, Eq, Ord, Data, Semigroup, Monoid, IsString)

getText :: Ident -> Text
getText (Ident txt) = txt

instance Pretty Ident where
    pretty (Ident name) = pretty name

instance Pretty Namespace where
    pretty (Namespace list) = concatWith (surround dot) $ fmap pretty list

intercalate :: Text -> [Ident] -> Ident
intercalate _ [] = error "INTERNAL ERROR: impossible"
intercalate t xs = Ident $ go $ fmap (\(Ident name) -> name) xs
  where
    go :: [Text] -> Text
    go [] = ""
    go [x] = x
    go (x : xs) = x <> t <> (go xs)

getOriginalName' :: Ident -> Names -> Ident
getOriginalName' name names =
    fromMaybe (error $ "INTERNAL ERROR: can't find name: " <> show name <> " in: " <> show names)
        $ Map.lookup name (unNames names)

getOriginalName :: Ident -> Names -> Maybe Ident
getOriginalName name names = Map.lookup name (unNames names)

existName :: Ident -> Names -> Bool
existName name names = Map.member name (unNames names)

insertName :: Ident -> Names -> Names
insertName name names = Names $ Map.insertWith (\_ x -> x) name name (unNames names)

renameBack :: (Data a) => Names -> a -> a
renameBack names = everywhere (mkT f)
  where
    f :: Ident -> Ident
    f name = fromMaybe name (getOriginalName name names)
