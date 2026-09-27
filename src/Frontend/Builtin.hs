{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

module Frontend.Builtin where

import Data.Map qualified as Map
import Data.Set qualified as Set
import Frontend.Types
    ( Forall,
      NoExtField (NoExtField),
      SourceInfo (SourceInfo),
      TyLit (..),
      Type (..),
      emptySpan, emptyTyParamList,
    )
import Names (Ident (..), Namespace (Namespace))
import Relude hiding (Type)
import Frontend.Typechecker.Types (Tc)

newtype Builtins a = Builtins (Map Namespace (Map Ident (Type a, SourceInfo)))

deriving instance (Forall Show a) => Show (Builtins a)

names :: Builtins a -> Set Ident
names (Builtins builtins) = Set.unions [Set.fromList (Map.keys el) | el <- Map.elems builtins]

isBuiltin :: Namespace -> Ident -> Builtins a -> Bool
isBuiltin namespace ident (Builtins m) = isJust $ Map.lookup ident =<< Map.lookup namespace m

lookup :: Namespace -> Ident -> Builtins a -> Maybe (Type a, SourceInfo)
lookup namespace name (Builtins m) = Map.lookup name =<< Map.lookup namespace m

builtins :: Builtins Tc
builtins =
    Builtins
        $ Map.singleton
            (Namespace ("std" :| []))
            ( Map.fromList
                [
                    ( Ident "printInt"
                    ,
                        ( TyFun emptyTyParamList [TyLit NoExtField Int] (TyLit NoExtField Unit)
                        , SourceInfo emptySpan "Built in"
                        )
                    )
                ,
                    ( Ident "printString"
                    ,
                        ( TyFun emptyTyParamList [TyLit NoExtField String] (TyLit NoExtField Unit)
                        , SourceInfo emptySpan "Built in"
                        )
                    )
                ,
                    ( Ident "printChar"
                    ,
                        ( TyFun emptyTyParamList [TyLit NoExtField Char] (TyLit NoExtField Unit)
                        , SourceInfo emptySpan "Built in"
                        )
                    )
                ]
            )
