{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Compile
import Control.Exception (assert, throw)
import Data.List (isSubsequenceOf)
import Data.Text (pack, unpack)
import Data.Text.IO qualified as Text
import Relude
import System.Directory
    ( doesDirectoryExist,
      getCurrentDirectory,
      listDirectory,
      withCurrentDirectory,
    )
import System.Exit (ExitCode (..))
import System.FilePath (takeExtension, (</>))
import System.Process (proc, readCreateProcessWithExitCode)
import Utils (File (..), conkFileExtension)

newtype Result = Result Bool
    deriving (Show)

data TestCase = TestCase
    { inputFiles :: NonEmpty File
    , outFile :: Maybe File
    , directoryPath :: FilePath
    , testType :: TestType
    }
    deriving (Show)

data TestType = Good | Bad
    deriving (Show)

newtype MissingDirectoryException = MissingDirectoryException FilePath
    deriving (Show)

instance Exception MissingDirectoryException

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> allTests
        xs -> mapM_ (\path -> specificTest (if "good" `isSubsequenceOf` path then Good else Bad) path) xs

specificTest :: TestType -> FilePath -> IO ()
specificTest testType path = do
    testCase <- readDirectory testType path
    result <- runTestCase testCase
    consumeResult result >>= \case
        True -> Relude.exitSuccess
        False -> Relude.exitFailure

allTests :: IO ()
allTests = do
    putStrLn "RUNNING ALL TESTS\n"
    goods <- sort . fmap ("test/good/" <>) <$> listDirectory "./test/good"
    bads <- sort . fmap ("test/bad/" <>) <$> listDirectory "./test/bad"
    goodResults <- mapM (readDirectory Good >=> runTestCase) goods
    badResults <- mapM (readDirectory Bad >=> runTestCase) bads
    mapM consumeResult goodResults >>= flip unless Relude.exitFailure . and
    mapM consumeResult badResults >>= flip unless Relude.exitFailure . and
    putStrLn "===== ALL TESTS PASSED ====="
    Relude.exitSuccess

readDirectory :: TestType -> FilePath -> IO TestCase
readDirectory testType dir = do
    dirExists <- doesDirectoryExist dir
    unless dirExists (throw (MissingDirectoryException dir))
    files <- listDirectory dir
    let inputFilepaths = filter ((conkFileExtension ==) . takeExtension) files
    assert (not (null inputFilepaths)) (pure ())
    let outputFilepaths = filter ((".out" ==) . takeExtension) files
    assert (length outputFilepaths <= 1) (pure ())
    let outputFilepath = listToMaybe outputFilepaths
    inputFiles <- mapM (\path -> File path . decodeUtf8 <$> readFileBS (dir </> path)) inputFilepaths
    outputFile <- mapM (\path -> File path . decodeUtf8 <$> readFileBS (dir </> path)) outputFilepath
    pure (TestCase (fromList inputFiles) outputFile dir testType)

consumeResult :: Result -> IO Bool
consumeResult = pure . coerce

runTestCase :: TestCase -> IO Result
runTestCase
    TestCase
        { inputFiles = inputFiles
        , outFile = outFile
        , directoryPath = directoryPath
        , testType = Good
        } =
        do
            putStrLn "=========================================================="
            putStrLn ("Running test '" <> directoryPath <> "'")
            withCurrentDirectory directoryPath $ do
                cwd <- getCurrentDirectory
                putStrLn $ "Setting current working directory to `" <> cwd <> "`"
                Text.putStrLn $ "Compiling " <> unwords ((\file -> pack file.name) <$> toList inputFiles)
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
                            >> pure (Result False)
                    ExitSuccess -> do
                        case outFile of
                            Nothing -> putStrLn "Missing out file" >> Relude.exitFailure
                            Just outFile -> do
                                if outFile.content == pack out
                                    then putStrLn ("Success for '" <> outFile.name <> "'") >> pure (Result True)
                                    else do
                                        Text.putStrLn $ "Expected: " <> clarifyEmpty outFile.content
                                        Text.putStrLn $ "Got: " <> clarifyEmpty (pack out)
                                        putStrLn ("Test: '" <> outFile.name <> "' failed with error message: " <> err)
                                        pure (Result False)
runTestCase TestCase {inputFiles = inputFiles, outFile = _, testType = testType} = do
    let (a, _) = runCompile inputFiles
    case (a, testType) of
        (Left reason, Bad) ->
            putStrLn
                ("Success for '" <> (head inputFiles).name <> "'. It failed with reason:\n" <> unpack reason)
                >> pure (Result True)
        (Right _, Bad) ->
            putStrLn ("Test: '" <> (head inputFiles).name <> "' failed because program compiled successfully.")
                >> pure (Result False)

clarifyEmpty :: Text -> Text
clarifyEmpty "" = "<empty>"
clarifyEmpty s = s
