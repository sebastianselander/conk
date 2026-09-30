module Backend.Core.Core where

import Backend.Core.Basic (basicCore)
import Backend.Core.Breaks (removeUnreachable, simplifyBreakAndReturn)
import Backend.Core.Types
import Frontend.Typechecker.Types qualified as Tc
import Names (Names)
import Relude

lowerToCore :: Names -> Tc.ProgramTc -> Program
lowerToCore names = removeUnreachable . simplifyBreakAndReturn . basicCore names
