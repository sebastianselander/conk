module Origin (Origin (..)) where

import Data.Data (Data)
import Relude

data Origin = Function | Lifted | ConstructorFn | Monomorphised
    deriving (Show, Eq, Ord, Data, Generic)
