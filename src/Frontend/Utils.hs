module Frontend.Utils where

import Frontend.Types (TyParamList (..), TyVar, SourceInfo)
import Relude

isUnique :: TyParamList -> Maybe (SourceInfo, TyVar)
isUnique Missing = Nothing
isUnique (Params loc xs) = go (reverse (toList xs))
  where
    go :: [TyVar] -> Maybe (SourceInfo, TyVar)
    go [] = Nothing
    go (x:xs) 
        | x `elem` xs = Just (loc, x)
        | otherwise = go xs
