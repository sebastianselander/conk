{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use concatMap" #-}

module Compile where

import Backend.Desugar.Desugar (desugar)
import Backend.Desugar.Pretty (prettyDesugar)
import Backend.Llvm.Llvm (assemble)
import Backend.Llvm.Lower (llvmOut)
import Control.Arrow (left)
import Control.Monad.Except (liftEither)
import Control.Monad.Writer (MonadWriter, Writer, runWriter, tell)
import Data.Set qualified as Set
import Data.Text (concat)
import Frontend.Error (Report (..))
import Frontend.Parser.Parse (parse)
import Frontend.Renamer.Rn (rename)
import Frontend.StatementCheck (check)
import Frontend.Tc (tc)
import Relude hiding (concatMap, concat, intercalate)
import Text.Pretty.Simple (pShow)
import Options(Pass(..))
import Frontend.Renamer.Pretty (prettyRenamer)
import Frontend.Typechecker.Pretty (pThing)

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

compile :: String -> Text -> ExceptT Text (Writer DebugOutputs) Text
compile fileName fileContents = do
    res <- liftEither $ left report $ parse fileName fileContents
    log (Debug Parse Nothing (toStrict $ pShow res)) []

    (res, names) <- liftEither $ left report $ rename res
    log (Debug Rename (Just $ prettyRenamer res) (toStrict $ pShow res)) []

    res <- liftEither $ left report $ check res
    log (Debug StCheck Nothing (toStrict $ pShow res)) []

    res <- case tc names res of
        (res, warnings) -> do
            res <- liftEither $ left report res
            log (Debug TypeCheck (Just $ pThing res) (toStrict $ pShow res)) (fmap report warnings)
            pure res

    res <- case desugar names res of
        res -> do
            log (Debug Desugar (Just $ prettyDesugar res) (toStrict $ pShow res)) []
            pure res

    case assemble res of
        res -> do
            log (Debug Llvm (Just $ llvmOut res) (toStrict $ pShow res)) []
            pure (llvmOut res)

runCompile :: String -> Text -> (Either Text Text, DebugOutputs)
runCompile fileName = runWriter . runExceptT . compile fileName

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
