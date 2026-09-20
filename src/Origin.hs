module Origin (Origin (..)) where

import Data.Data (Data)
import Relude

data Origin = Top | Lifted | ConstructorFn
    deriving (Show, Eq, Ord, Data, Generic)
