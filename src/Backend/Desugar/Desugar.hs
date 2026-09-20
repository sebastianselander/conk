module Backend.Desugar.Desugar where

import Backend.Desugar.Basic (basicDesugar)
import Backend.Desugar.Breaks (removeUnreachable, simplifyBreakAndReturn)
import Backend.Desugar.Types
import Frontend.Typechecker.Types qualified as Tc
import Names (Names)
import Relude

desugar :: Names -> Tc.ProgramTc -> Program
desugar names = removeUnreachable . simplifyBreakAndReturn . basicDesugar names
