{-# LANGUAGE ApplicativeDo #-}

module Options (cmdlineParser, Options(..), Pass(..)) where

import Data.Set qualified as Set
import Options.Applicative
import Relude
import Options.Applicative.NonEmpty (some1)

data Pass = Parse | Rename | StCheck | TypeCheck | Desugar | Llvm
    deriving (Show, Ord, Eq)

data Options = Options
    { dumps :: Set Pass
    , filepaths :: NonEmpty FilePath
    }

cmdlineParser :: IO Options
cmdlineParser = execParser (info (options <**> helper) fullDesc)

options :: Parser Options
options = do
    dumps <- pDumps
    filepaths <- pInput
    pure $ Options {dumps, filepaths}

pDumps :: Parser (Set Pass)
pDumps =
    Set.fromList
        . catMaybes
        <$> sequenceA
            [ flag Nothing (Just Parse) (long "dump-ps" <> help "Show the output of the parser")
            , flag Nothing (Just Rename) (long "dump-rn" <> help "Show the output of the renamer")
            , flag
                Nothing
                (Just StCheck)
                (long "dump-st" <> help "Show the output of the statement checking phase")
            , flag Nothing (Just TypeCheck) (long "dump-tc" <> help "Show the output of the typechecker")
            , flag Nothing (Just Desugar) (long "dump-ds" <> help "Show the output of the desugaring phase")
            , flag Nothing (Just Llvm) (long "dump-llvm" <> help "Dump the generated llvm-ir code")
            ]

pInput :: Parser (NonEmpty FilePath)
pInput = some1 (argument str (metavar "[FILE...]"))
