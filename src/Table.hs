{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Table where

import Control.Lens.TH
import Relude hiding (Type)

import Frontend.Builtin (Builtins)
import Frontend.Types (Forall)
import Names (Ident, Namespace)


data DefTable phase a info = Table
    { _builtIns :: Builtins phase
    , _functions :: Map Namespace (Map Ident (a, info))
    , _types :: Map Namespace (Map Ident (a, info))
    , _constructors :: Map Namespace (Map Ident (a, info))
    }


deriving instance (Forall Show phase, Show a, Show info) => Show (DefTable phase a info)


$(makeLenses ''DefTable)
