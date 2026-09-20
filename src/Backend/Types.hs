{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}

module Backend.Types where

import Data.Data (Data)
import Names (Ident)
import Relude hiding (Type)

data Type
    = I Int
    | Mut Type
    | Float
    | PointerType Type
    | OpaquePointer
    | Void
    | StructType [Type]
    | ArrayType Int Type
    | TyFun [Type] Type
    | TyCon Ident
    deriving (Show, Eq, Ord, Data)

sizeOf :: Type -> Integer
sizeOf = \case
    I n -> fromIntegral $ max (n `div` 8) 8
    Float -> 8
    PointerType _ -> 8
    OpaquePointer -> 8
    Void -> 8
    StructType types -> sum (fmap sizeOf types)
    ArrayType n ty -> fromIntegral n * sizeOf ty
    TyFun _ _ -> 8
    TyCon _ -> 8 -- NOTE: Must be behind pointer
    Mut ty -> sizeOf ty

pattern Unit :: Type
pattern Unit <- I 1
    where
        Unit = I 1

pattern Bool :: Type
pattern Bool <- I 1
    where
        Bool = I 1

pattern Char :: Type
pattern Char <- I 8
    where
        Char = I 8

pattern Int64 :: Type
pattern Int64 <- I 64
    where
        Int64 = I 64

pattern Int32 :: Type
pattern Int32 <- I 32
    where
        Int32 = I 32

pattern String :: Type
pattern String <- (PointerType (I 8))
    where
        String = PointerType (I 8)

ptr :: Type -> Type
ptr = PointerType

opaquePtr :: Type
opaquePtr = OpaquePointer
