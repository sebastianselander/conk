module Backend.Core.Breaks where

import Data.Generics (everywhere, mkT)
import Relude

import Backend.Core.Types

removeUnreachable :: Program -> Program
removeUnreachable = everywhere (mkT go)
  where
    go :: [Statement] -> [Statement]
    go = takeWhilePlusOne (\x -> not (isBreak x || isReturn x))

isBreak :: Statement -> Bool
isBreak Break = True
isBreak _ = False

isReturn :: Statement -> Bool
isReturn (Return _) = True
isReturn _ = False

takeWhilePlusOne :: (a -> Bool) -> [a] -> [a]
takeWhilePlusOne f = go
  where
    go [] = []
    go (x : xs)
        | f x = x : go xs
        | otherwise = [x]
