module Backend.Core.Core where

import Relude

import Backend.Core.Basic (basicCore)
import Backend.Core.Breaks (removeUnreachable, simplifyBreakAndReturn)
import Backend.Core.Types
import Names (Names)

import Frontend.Typechecker.Types qualified as Tc


lowerToCore :: Names -> Tc.ProgramTc -> Program
lowerToCore names =
    removeUnreachable
        . simplifyBreakAndReturn
        . basicCore names
