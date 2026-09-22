module Frontend.Phase where

import Data.Data (Data)

data Phase = NoPhase | Par | Rn | Tc
    deriving Data
