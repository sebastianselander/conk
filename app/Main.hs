module Main where

import Compile (produceExecutable)
import Options (Options (..), cmdlineParser)
import Relude hiding (null, zipWith)
import Utils (File (..))
import Data.List.NonEmpty.Extra (zipWith)
import System.FilePath (normalise)

main :: IO ()
main = do
    Options {filepaths, dumps} <- cmdlineParser
    filepaths <- pure (normalise <$> filepaths)
    contents <- mapM readFileBS filepaths
    let files = zipWith (\filename content -> File filename (decodeUtf8 content)) filepaths contents
    executable <- produceExecutable dumps files "main"
    putStrLn ("Produced executable '" <> executable <> "'")
