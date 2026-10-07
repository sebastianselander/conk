{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use concatMap" #-}

module Compile where

import Control.Arrow (left)
import Control.Monad.Except (liftEither)
import Control.Monad.Writer (MonadWriter, Writer, WriterT, runWriter, runWriterT, tell)
import Data.Foldable1 (foldr1)
import Data.Text (concat, pack)
import Data.Text.IO (hPutStrLn)
import Relude hiding (concat, concatMap, intercalate)
import System.Directory.Extra
    ( createDirectory,
      doesDirectoryExist,
      removeDirectoryRecursive,
      withCurrentDirectory,
    )
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
import Text.Pretty.Simple (pShow)

import Data.Functor qualified as Functor
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text.IO qualified as Text

import Backend.Core.Core (lowerToCore)
import Backend.Core.Pretty (prettyCore)
import Backend.Llvm.Llvm (assemble)
import Backend.Llvm.Lower (llvmOut)
import Backend.Llvm.Prelude (prelude)
import Backend.Llvm.Types (Ir, updateDecls)
import Frontend.Builtin (builtins)
import Frontend.Error (Report (..), TcError, TcWarning)
import Frontend.MonomorphizerCollector (collect)
import Frontend.Parser.Parse (parse)
import Frontend.Parser.Types (Par)
import Frontend.Renamer.Pretty (prettyRenamer)
import Frontend.Renamer.Rn (rename)
import Frontend.StatementCheck (check)
import Frontend.Tc2 (TypeCons (..), getFuns, getTypesAndCons, typecheck)
import Frontend.Typechecker.Pretty (pThing)
import Frontend.Typechecker.Types (ProgramTc, Tc)
import Frontend.Types (Adt (Adt), Def (..), Fn (Fn), Program (Program))
import Names (Ident (..), Namespace (Namespace), combine)
import Options (Pass (..))
import Table (DefTable (..))
import Utils (File (name), zipNE)


data DebugOutput = Debug {phase :: Pass, prettyTxt :: Maybe Text, normalTxt :: Text}


data DebugOutputs = Debugs {debugs :: [DebugOutput], warnings :: [Text]}


instance Semigroup DebugOutputs where
    (<>) (Debugs l1 r1) (Debugs l2 r2) = Debugs (l1 <> l2) (r1 <> r2)


instance Monoid DebugOutputs where
    mempty = Debugs [] []
    mappend = (<>)


log :: (MonadWriter DebugOutputs m, MonadIO m) => Set Pass -> DebugOutput -> [Text] -> m ()
log passes_to_log debug@(Debug phase _ _) warnings = do
    when (Set.member phase passes_to_log) $ liftIO $ Text.putStrLn (showDebug debug)
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
        DefFn (Fn _ name _ _ _ _) -> Just name
        DefAdt (Adt _ name _) -> Just name
        DefImport _ -> Nothing


compile :: Set Pass -> NonEmpty File -> ExceptT Text (WriterT DebugOutputs IO) (NonEmpty Ir)
compile passes files = do
    programs <- liftEither $ left report $ mapM parse files
    log passes (Debug Parse Nothing (toStrict $ pShow programs)) []

    let modules = NE.zip files programs
    let symbolsMap = gatherSymbols modules
    let namespaces = Set.fromList $ toList $ fmap (\(Program namespace _) -> namespace) programs

    res <- liftEither $ left report $ mapM (rename namespaces symbolsMap) programs
    let (programs, names) = second (foldr1 combine) (Functor.unzip res)
    log passes (Debug Rename (Just $ prettyRenamer programs) (toStrict $ pShow res)) []

    res <- liftEither $ left report $ mapM check programs
    log passes (Debug StCheck Nothing (toStrict $ pShow res)) []

    let defTable =
            Table
                builtins
                (Map.unions $ fmap (\(Program ns defs) -> Map.singleton ns (Map.fromList (getFuns defs))) res)
                ( Map.unions
                    $ fmap (\(Program ns defs) -> Map.singleton ns (Map.fromList (types (getTypesAndCons defs)))) res
                )
                ( Map.unions
                    $ fmap (\(Program ns defs) -> Map.singleton ns (Map.fromList (cons (getTypesAndCons defs)))) res
                )
    programs <- case fmap (typecheck defTable names) res of
        xs ->
            let single ::
                    (Either [TcError] ProgramTc, [TcWarning]) -> ExceptT Text (WriterT DebugOutputs IO) ProgramTc
                single x =
                    case x of
                        (res, warnings) -> do
                            res <- liftEither $ left report res
                            log passes (Debug TypeCheck (Just $ pThing res) (toStrict $ pShow res)) (fmap report warnings)
                            pure res
             in mapM single xs

    let _collections = fmap collect programs


    res <- case fmap (lowerToCore names) programs of
        res -> forM res $ \res -> do
            log passes (Debug Core (Just $ prettyCore res) (toStrict $ pShow res)) []
            pure res

    case fmap assemble res of
        res -> forM res $ \res -> do
            log passes (Debug Llvm (Just $ llvmOut res) (toStrict $ pShow res)) []
            pure res


runCompile :: Set Pass -> NonEmpty File -> IO (Either Text (NonEmpty Ir), DebugOutputs)
runCompile passes = runWriterT . runExceptT . compile passes


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
    hPutStrLn stderr $ "Linking: " <> show (toList files)
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


produceExecutable ::
    (HasCallStack) => Set Pass -> NonEmpty File -> FilePath -> IO FilePath
produceExecutable passes files out = do
    (res, _) <- runCompile passes files
    case res of
        Left err -> do
            hPutStrLn stderr err
            exitFailure
        Right prg -> do
            let buildDir = "build"
            exists <- doesDirectoryExist buildDir
            when exists $ removeDirectoryRecursive buildDir
            createDirectory buildDir
            preludeFile <- produceAsmFile "prelude.asm" (Left (snd prelude))
            asmFiles <-
                mapM
                    (\(file, prg) -> produceAsmFile (replaceExtension file.name "asm") (Right prg))
                    (zipNE files prg)
            objFiles <-
                mapM (\file -> produceObjectFile file (replaceExtension file "o")) (preludeFile :| toList asmFiles)
            linkObjectFiles objFiles (buildDir </> out)


showDebug :: DebugOutput -> Text
showDebug (Debug phase pretty normal) =
    unlines
        [ "======== " <> show phase <> " output ========"
        , ""
        , normal
        , fromMaybe "" pretty
        , ""
        ]


showDebugs :: Set Pass -> DebugOutputs -> Text
showDebugs dumps (Debugs debugs warnings) =
    concat
        (fmap (\dbg@(Debug phase _ _) -> if Set.member phase dumps then showDebug dbg else "") debugs)
        <> concat warnings
