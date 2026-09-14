{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use concatMap" #-}

module Compile where

import Backend.Desugar.Desugar (desugar)
import Backend.Desugar.Pretty (prettyDesugar)
import Backend.Llvm.Llvm (assemble)
import Backend.Llvm.Lower (llvmOut)
import Backend.Llvm.Prelude (prelude)
import Backend.Llvm.Types (Ir, updateDecls)
import Control.Arrow (left)
import Control.Monad.Except (liftEither)
import Control.Monad.Writer (MonadWriter, Writer, runWriter, tell)
import Data.Foldable1 (foldr1)
import Data.Functor qualified as Functor
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text (concat, pack, intercalate)
import Data.Text.IO (hPutStrLn)
import Frontend.Builtin (builtIns)
import Frontend.Error (Report (..), TcError, TcWarning)
import Frontend.Parser.Parse (parse)
import Frontend.Parser.Types (Par)
import Frontend.Renamer.Pretty (prettyRenamer)
import Frontend.Renamer.Rn (rename)
import Frontend.Renamer.Types (Boundedness (Imported))
import Frontend.StatementCheck (check)
import Frontend.Tc (tc)
import Frontend.Typechecker.Pretty (pThing)
import Frontend.Typechecker.Types (ProgramTc)
import Frontend.Types (Adt (Adt), Def (..), Fn (Fn), Program (Program))
import Names (Ident (..), combine, Namespace (Namespace))
import Options (Pass (..))
import Relude hiding (concat, concatMap, intercalate)
import System.Directory.Extra (createDirectory, removeDirectoryRecursive)
import System.Exit (ExitCode (..))
import System.FilePath
    ( dropExtension,
      replaceDirectory,
      replaceExtension,
      splitDirectories,
      takeBaseName,
      (</>),
    )
import System.Process.Extra (proc, readCreateProcessWithExitCode)
import Table (DefTable (..))
import Text.Pretty.Simple (pShow)
import Utils (File (name), zipNE)
import qualified Data.Text.Prettyprint.Doc as Pretty
import qualified Frontend.Renamer.Pretty as Pretty

data DebugOutput = Debug {phase :: Pass, prettyTxt :: Maybe Text, normalTxt :: Text}
data DebugOutputs = Debugs {debugs :: [DebugOutput], warnings :: [Text]}

instance Semigroup DebugOutputs where
    (<>) (Debugs l1 r1) (Debugs l2 r2) = Debugs (l1 <> l2) (r1 <> r2)

instance Monoid DebugOutputs where
    mempty = Debugs [] []
    mappend = (<>)

log :: (MonadWriter DebugOutputs m) => DebugOutput -> [Text] -> m ()
log debug warnings = do
    tell (Debugs [debug] warnings)

-- TODO(sebsel): Figure out better name
gatherSymbols :: NonEmpty (File, Program Par) -> Map Ident Namespace
gatherSymbols =
    foldl'
        ( \acc (file, Program _ defs) ->
            foldr
                (\def -> Map.insert def (Namespace $ fromList (pack <$> splitDirectories (dropExtension file.name))))
                acc
                (mapMaybe nameOf defs)
        )
        mempty
  where
    nameOf :: Def Par -> Maybe Ident
    nameOf = \case
        DefFn (Fn _ name _ _ _) -> Just name
        DefAdt (Adt _ name _) -> Just name
        DefImport _ -> Nothing

compile :: NonEmpty File -> ExceptT Text (Writer DebugOutputs) (NonEmpty Ir)
compile files = do
    programs <- liftEither $ left report $ mapM parse files
    log (Debug Parse Nothing (toStrict $ pShow programs)) []

    let modules = NE.zip files programs
    let symbolsMap = Map.map (Imported,) $ gatherSymbols modules

    res <- liftEither $ left report $ mapM (rename symbolsMap) programs
    let (programs, names) = second (foldr1 combine) (Functor.unzip res)
    log (Debug Rename (Just $ prettyRenamer programs) (toStrict $ pShow res)) []

    res <- liftEither $ left report $ mapM check programs
    log (Debug StCheck Nothing (toStrict $ pShow res)) []

    let defTable = Table mempty mempty mempty mempty

    let x = intercalate "\n\n" $ toList $ fmap Pretty.prettyRenamer res

    programs <- case error x of -- case fmap (tc defTable names) res of
        xs ->
            let single :: (Either [TcError] ProgramTc, [TcWarning]) -> ExceptT Text (Writer DebugOutputs) ProgramTc
                single x =
                    case x of
                        (res, warnings) -> do
                            res <- liftEither $ left report res
                            log (Debug TypeCheck (Just $ pThing res) (toStrict $ pShow res)) (fmap report warnings)
                            pure res
             in mapM single xs

    res <- case fmap (desugar names) programs of
        res -> forM res $ \res -> do
            log (Debug Desugar (Just $ prettyDesugar res) (toStrict $ pShow res)) []
            pure res

    case fmap assemble res of
        res -> forM res $ \res -> do
            log (Debug Llvm (Just $ llvmOut res) (toStrict $ pShow res)) []
            pure res

runCompile :: NonEmpty File -> (Either Text (NonEmpty Ir), DebugOutputs)
runCompile = runWriter . runExceptT . compile

produceAsmFile :: FilePath -> Either Text Ir -> IO FilePath
produceAsmFile asmFilename ir = do
    ir <- pure $ fmap (updateDecls (fst prelude <>)) ir
    let llFile = "./build/" <> replaceExtension (takeBaseName asmFilename) "ll"
    case ir of
        Right ir -> writeFileText llFile (llvmOut ir)
        Left raw -> writeFileText llFile raw
    let out = replaceDirectory asmFilename "./build/"
    let process = proc "llc" [llFile, "-o", out]
    (code, _, err) <- readCreateProcessWithExitCode process ""
    case code of
        ExitSuccess -> do
            pure out
        ExitFailure code -> do
            hPutStrLn stderr ("Failed producing asm file: " <> pack out)
            hPutStrLn stderr (pack err)
            exitWith (ExitFailure code)

produceObjectFile :: FilePath -> FilePath -> IO FilePath
produceObjectFile asmFilename objectFilename = do
    let process = proc "as" ["--64", asmFilename, "-o", objectFilename]
    (code, _, err) <- readCreateProcessWithExitCode process ""
    case code of
        ExitSuccess -> pure objectFilename
        ExitFailure code -> do
            hPutStrLn stderr ("Failed producing object file: " <> pack objectFilename)
            hPutStrLn stderr (pack err)
            exitWith (ExitFailure code)

linkObjectFiles :: NonEmpty FilePath -> FilePath -> IO FilePath
linkObjectFiles files out = do
    hPutStrLn stderr $ "Linking: " <> show files
    let process =
            proc
                "gcc"
                ( ["-no-pie", "-o", out]
                    <> toList files
                )
    (code, _, err) <- readCreateProcessWithExitCode process ""
    case code of
        ExitSuccess -> pure out
        ExitFailure code -> do
            hPutStrLn stderr ("Failed producing executable: " <> pack out)
            hPutStrLn stderr (pack err)
            exitWith (ExitFailure code)

produceExecutable :: Set Pass -> NonEmpty File -> FilePath -> IO FilePath
produceExecutable dumps files out = do
    case runCompile files of
        (res, debugs) -> case res of
            Left err -> do
                hPutStrLn stderr err
                exitFailure
            Right prg -> do
                let debug = showDebugs dumps debugs
                case debug of
                    "" -> pure ()
                    _ -> hPutStrLn stderr debug
                let buildDir = "build"
                removeDirectoryRecursive buildDir
                createDirectory buildDir
                preludeFile <- produceAsmFile "prelude.asm" (Left (snd prelude))
                asmFiles <-
                    mapM
                        (\(file, prg) -> produceAsmFile (replaceExtension file.name "asm") (Right prg))
                        (zipNE files prg)
                objFiles <-
                    mapM (\file -> produceObjectFile file (replaceExtension file "o")) (preludeFile :| toList asmFiles)
                linkObjectFiles objFiles (buildDir </> out)

showDebug :: Set Pass -> DebugOutput -> Text
showDebug dumps (Debug phase pretty normal) =
    if Set.member phase dumps
        then
            unlines
                [ "======== " <> show phase <> " output ========"
                , ""
                , normal
                , fromMaybe "" pretty
                , ""
                ]
        else ""

showDebugs :: Set Pass -> DebugOutputs -> Text
showDebugs dumps (Debugs debugs warnings) = concat (fmap (showDebug dumps) debugs) <> concat warnings
