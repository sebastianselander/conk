{-# LANGUAGE TemplateHaskell #-}

module Table where

import Control.Lens.TH
import Names (Ident, Namespace)
import Relude
import Frontend.Builtin (Builtins)

data DefTable a info = Table
    { _builtIns :: Builtins ()
    , _functions :: Map Namespace (Map Ident (a, info))
    , _types :: Map Namespace (Map Ident (a, info))
    , _constructors :: Map Namespace (Map Ident (a, info))
    } deriving Show


$(makeLenses ''DefTable)
