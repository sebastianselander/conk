{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Compile
import Data.List (groupBy)
import Data.List.NonEmpty qualified as NE
import Data.Text (pack)
import Data.Text.IO qualified as Text
import Relude
import System.Directory (listDirectory)
import System.Exit (ExitCode (..))
import System.FilePath (dropExtensions)
import System.Process (proc, readCreateProcessWithExitCode)
import Utils (File (..), mkFile)

main :: IO ()
main = do
    putStrLn "RUNNING TESTS\n"
    goods <-
        ( fmap
            ( \case
                [a, b] -> (a, b)
                [a] -> error $ "missing file for: " <> pack a
                _ -> error "incorrect amount of files"
            )
            . groupBy (\l r -> dropExtensions l == dropExtensions r)
            . sort
        )
            . fmap ("test/good/" ++)
            <$> listDirectory "test/good"

    bads <-
        sort
            . fmap ("test/bad/" ++)
            <$> listDirectory "test/bad"
    goods <-
        mapM (testFile isRight) =<< mapM (mkFiles . first NE.singleton) goods
    bads <- mapM (testFile isLeft . (,Nothing) . NE.singleton) =<< mapM mkFile bads
    unless (and goods && and bads) exitFailure
    exitSuccess

mkFiles :: (NonEmpty String, String) -> IO (NonEmpty File, Maybe File)
mkFiles (a, b) = do
    afiles <- mapM mkFile a
    bfile <- mkFile b
    pure (afiles, Just bfile)

testFile :: (forall a b. Either a b -> Bool) -> (NonEmpty File, Maybe File) -> IO Bool
testFile _ (inputFiles, Just outputFile) = do
    putStrLn "=========================================================="
    putStrLn ("Running test for '" <> outputFile.name <> "'")
    executable <- produceExecutable mempty inputFiles "main"
    (code, out, err) <- readCreateProcessWithExitCode (proc executable []) ""
    case code of
        ExitFailure _ ->
            putStrLn
                ( "Test: '"
                    <> intercalate ":" (fmap (.name) (toList inputFiles))
                    <> "' failed with message: "
                    <> err
                )
                >> pure False
        ExitSuccess -> do
            if outputFile.content == pack out
                then putStrLn ("Success for '" <> outputFile.name <> "'") >> pure True
                else do
                    Text.putStrLn $ "Expected: " <> onEmpty outputFile.content
                    Text.putStrLn $ "Got: " <> onEmpty (pack out)
                    putStrLn ("Test: '" <> outputFile.name <> "' failed with error message: " <> err)
                    pure False
testFile isEither (inputFiles, Nothing) = do
    let (a, _) = runCompile inputFiles
    if isEither a
        then putStrLn ("Success for '" <> (head inputFiles).name <> "'") >> pure True
        else putStrLn ("Test: '" <> (head inputFiles).name <> "' failed.") >> pure False

onEmpty :: Text -> Text
onEmpty "" = "<empty>"
onEmpty s = s
