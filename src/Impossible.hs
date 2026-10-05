module Impossible where

import Control.Exception (throw)
import GHC.Show (Show (show))
import GHC.Stack (popCallStack)
import Relude


newtype Impossible = Impossible CallStack


instance Show Impossible where
    show (Impossible loc) =
        "An internal error has occured. Please report this as a bug. \
        \ Location of the error: "
            <> prettyCallStack loc


instance Exception Impossible


__IMPOSSIBLE__ :: (HasCallStack) => a
__IMPOSSIBLE__ = withNBackCallStack 0 $ throw . Impossible


withNBackCallStack :: (HasCallStack) => Word -> (CallStack -> b) -> b
withNBackCallStack n f = f (popnCallStack n from)
  where
    -- This very line (always dropped):
    here = callStack
    -- The invoker (n = 0):
    from = popCallStack here


{-| Pops n entries off a @CallStack@ using @popCallStack@.
Note that frozen callstacks are unaffected.
-}
popnCallStack :: Word -> CallStack -> CallStack
popnCallStack 0 = id
popnCallStack n = popnCallStack (n - 1) . popCallStack
