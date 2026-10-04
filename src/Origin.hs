module Origin (Origin (..)) where

import Data.Data (Data)
import Relude


data Origin = Function | Lifted | ConstructorFn | Monomorphised
    deriving (Data, Eq, Generic, Ord, Show)
