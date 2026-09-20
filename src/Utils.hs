{-# LANGUAGE OverloadedStrings #-}

module Utils where

import Data.Generics (Data, everything, everywhere, mkQ, mkT)
import Relude
import qualified Data.Text as Text

conkFileExtension :: IsString s => s
conkFileExtension = ".ck"

data File = File { name :: String, content :: Text }
    deriving (Show)

mkFile :: String -> IO File
mkFile name = File name . decodeUtf8 <$> readFileBS name

genMap :: (Data a, Typeable b) => (b -> b) -> a -> a
genMap f = everywhere (mkT f)

listify' :: (Data a, Typeable b) => (b -> Maybe c) -> a -> [c]
listify' f = everything (++) ([] `mkQ` (maybeToList . f))

both :: (Bifunctor p) => (c -> d) -> p c c -> p d d
both f = bimap f f

chain :: (a -> a -> b) -> a -> [a] -> [b]
chain _ _ [] = []
chain f base [x] = [f base x]
chain f base (x:y:xs) = f base x : go (x:y:xs)
  where
    go (x:y:xs) = f x y : go (y:xs)
    go _ = []


indent :: Int -> Text -> Text
indent n t = Text.replicate n " " <> t

quote :: Text -> Text
quote txt = "'" <> txt <> "'"

mapWithIndex :: (Traversable t, Num index) => (index -> a -> b) -> t a -> t b
mapWithIndex f = snd . mapAccumL (\index a -> (index + 1, f index a)) 0

mapWithIndexM :: (Traversable t, Monad m, Num index) => (index -> a -> m b) -> t a -> m (t b)
mapWithIndexM f = sequence . snd . mapAccumL (\index a -> (index + 1, f index a)) 0

catMaybesSnd :: [(a, Maybe b)] -> [(a,b)]
catMaybesSnd [] = []
catMaybesSnd ((_, Nothing) : xs) = catMaybesSnd xs
catMaybesSnd ((a, Just b) : xs) = (a, b) : catMaybesSnd xs

zipNE :: NonEmpty a -> NonEmpty b -> NonEmpty (a,b)
zipNE (a :| as) (b :| bs) = (a,b) :| Relude.zip as bs
