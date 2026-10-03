{-

This pass makes sure polymorphic functions' arguments are passed as pointers and that the result is then dereferenced.

-}

module Backend.Core.Polymorphism where

import Backend.Core.Types (Expr (App), Program, TyExpr (Typed), typeOf)
import Backend.Types (Type (OpaquePointer, TyFun, PointerType))
import Data.Generics (everywhere, mkT)
import Relude hiding (Type)

applyIndirection :: Program -> Program
applyIndirection = everywhere (mkT indirection)

-- This should not apply to polymorphic functions
indirection :: TyExpr -> TyExpr
indirection og@(Typed exprTy (App func args)) = case func.typeOf of
    TyFun argTys retTy ->
        let fixedArgs = zipWith mallocIfOpaque argTys args
         in case retTy of
                OpaquePointer ->
                    Typed
                        exprTy
                        (Dereference (Typed (PointerType exprTy) (App func fixedArgs)))
                _
                    | otherwise {- retTy == exprTy -} -> Typed exprTy (App func fixedArgs)
                    | otherwise ->
                        error
                            $ "INTERNAL ERROR: return type of function and expression do not match: `"
                            <> show retTy
                            <> "` | `"
                            <> show exprTy
                            <> "`"
    _ -> og
indirection expr = expr

mallocIfOpaque :: Type -> TyExpr -> TyExpr
mallocIfOpaque ty expr = case ty of
    OpaquePointer -> Typed (PointerType expr.typeOf) (Malloc expr)
    _ -> expr

