module Main where

import Compile (produceExecutable)
import Options (Options (..), cmdlineParser)
import Relude hiding (null)
import Utils (File (..))

main :: IO ()
main = do
    Options {filepath, dumps} <- cmdlineParser
    contents <- decodeUtf8 <$> readFileBS filepath
    let files = File filepath contents :| []
    executable <- produceExecutable dumps files "main"
    putStrLn ("Produced executable '" <> executable <> "'")
