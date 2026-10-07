{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}
module Main where

import Data.List.NonEmpty.Extra (zipWith)
import Data.Text (pack, stripPrefix, unpack)
import Relude hiding (null, zipWith)
import System.FilePath (normalise)

import Compile (produceExecutable)
import Options (Options (..), cmdlineParser)
import Utils (File (..))


main :: IO ()
main = do
    Options {filepaths, dumps, source_dir} <- cmdlineParser
    filepaths <- pure (normalise <$> filepaths)
    contents <- mapM readFileBS filepaths
    let files =
            zipWith
                (\filename content -> File (maybe id strip_directory_prefix source_dir filename) (decodeUtf8 content))
                filepaths
                contents
    executable <- produceExecutable dumps files "main"
    putStrLn ("Produced executable '" <> executable <> "'")


strip_directory_prefix :: FilePath -> FilePath -> FilePath
strip_directory_prefix directory path = maybe path unpack (stripPrefix "/" =<< stripPrefix (pack directory) (pack path))
