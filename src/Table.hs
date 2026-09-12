{-# LANGUAGE TemplateHaskell #-}

module Table where

import Control.Lens.TH
import Names (Ident, Namespace)
import Relude

data DefTable a info = Table
    { _builtIns :: Map Ident (a, info)
    , _functions :: Map Namespace (Map Ident (a, info))
    , _types :: Map Namespace (Map Ident (a, info))
    , _constructors :: Map Namespace (Map Ident (a, info))
    }
    deriving (Show, Ord, Eq)

$(makeLenses ''DefTable)
