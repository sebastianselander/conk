module Backend.Desugar.Breaks where

import Backend.Desugar.Types
import Data.Generics (everywhere, mkT)
import Relude

simplifyBreakAndReturn :: Program -> Program
simplifyBreakAndReturn = everywhere (mkT go)
  where
    go :: TyExpr -> TyExpr
    go (Typed ty expr) = case expr of
        If condition@(Typed _ty Break) _true _false -> condition
        If condition@(Typed _ty (Return _)) _true _false -> condition
        While condition@(Typed _ty Break) _body -> condition
        While condition@(Typed _ty (Return _)) _body -> condition
        e -> Typed ty e

removeUnreachable :: Program -> Program
removeUnreachable = everywhere (mkT go)
  where
    go :: [TyExpr] -> [TyExpr]
    go = takeWhilePlusOne (\x -> not (isBreak x || isReturn x))

isBreak :: TyExpr -> Bool
isBreak (Typed _ Break) = True
isBreak _ = False

isReturn :: TyExpr -> Bool
isReturn (Typed _ (Return _)) = True
isReturn _ = False

takeWhilePlusOne :: (a -> Bool) -> [a] -> [a]
takeWhilePlusOne f = go
  where
    go [] = []
    go (x : xs)
        | f x = x : go xs
        | otherwise = [x]
