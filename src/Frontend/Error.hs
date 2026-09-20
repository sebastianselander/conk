{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Frontend.Error where

import Control.Lens.Getter (view)
import Control.Monad.Validate
import Data.Text (intercalate, pack)
import Data.Text qualified as Text
import Frontend.Parser.Pretty ()
import Frontend.Parser.Types (ImportPar)
import Frontend.Parser.Utils (CustomParseError)
import Frontend.Renamer.Pretty ()
import Frontend.Renamer.Types (ExprRn, PatternRn)
import Frontend.TH
import Frontend.Typechecker.Ctx (Ctx, exprStack, names)
import Frontend.Typechecker.Pretty (pThing)
import Frontend.Typechecker.Types
import Frontend.Types (SourceInfo (..), Span (..))
import Names (Ident, Namespace, getOriginalName', renameBack)
import Relude hiding (All, First, intercalate)
import Text.Megaparsec (unPos)
import Text.Megaparsec.Error (ParseErrorBundle, errorBundlePretty)
import Utils (indent, quote)

data RnError
    = UnboundVariable SourceInfo Ident
    | ConflictingDefinitionArgument SourceInfo Ident
    | DuplicateToplevels SourceInfo Ident
    | UnboundImport SourceInfo Namespace
    deriving (Show)

data TcError
    = TyExpectedGot SourceInfo [ExprRn] [TypeTc] TypeTc
    | ImmutableVariable SourceInfo [ExprRn] Ident
    | EmptyReturnNonUnit SourceInfo [ExprRn] TypeTc
    | ApplyNonFunction SourceInfo [ExprRn] TypeTc
    | PartiallyAppliedFunction SourceInfo [ExprRn] Int Int
    | TooManyArguments SourceInfo [ExprRn] Int Int
    | AssignNonVariable SourceInfo [ExprRn] Ident
    | ExpectingImmutable SourceInfo [ExprRn]
    | TypeMustBeKnown SourceInfo [ExprRn] Ident
    | ExpectedTyGotLambda SourceInfo [ExprRn] TypeTc
    | ExpectedLambdaNArgs SourceInfo [ExprRn] Int Int
    | ExpectedPatNArgs SourceInfo [ExprRn] PatternRn Int Int
    deriving (Show)

data ChError
    = BreakOutsideLoop SourceInfo
    | MissingReturn SourceInfo Ident
    | UnreachableStatement SourceInfo
    deriving (Show)

data TcWarning = MakeExpressionBreak SourceInfo ExprTc
    deriving (Show)

class Report a where
    report :: a -> Text

instance Report (ParseErrorBundle Text CustomParseError) where
    report = pack . errorBundlePretty

instance (Report a) => Report [a] where
    report xs = intercalate "\n\n" $ fmap report xs

instance Report RnError where
    report = reportRnError

instance Report TcError where
    report = reportTcError

instance Report TcWarning where
    report = show

instance Report ChError where
    report = show

reportRnError :: RnError -> Text
reportRnError err = case err of
    UnboundVariable info name ->
        combineRn
            info
            (unwords ["Variable", quote $ pThing name, "not in scope"])
    ConflictingDefinitionArgument info name -> combineRn info (unwords ["Conflicting definitions for", quote $ pThing name])
    DuplicateToplevels info name -> combineRn info (unwords ["Definition", quote $ pThing name, "already declared earlier"])
    UnboundImport loc namespace -> combineRn loc (unwords ["Import", quote $ pThing namespace, "does not exist"])

reportTcError :: TcError -> Text
reportTcError err = case err of
    TyExpectedGot info currExpr expecteds got ->
        combineTc
            info
            currExpr
            ( unwords
                [ "Expected type"
                , intercalate ", " $ fmap (quote . pThing) expecteds
                , "but got"
                , quote (pThing got)
                ]
            )
    ImmutableVariable info currExpr name ->
        combineTc
            info
            currExpr
            (unwords ["Can not mutate immutable variable", quote (pThing name)])
    EmptyReturnNonUnit info currExpr ty ->
        combineTc
            info
            currExpr
            (unwords ["Expected a return type of", quote (pThing ty), "but an empty return has type ()"])
    ApplyNonFunction info currExpr ty ->
        combineTc
            info
            currExpr
            (unwords ["Can not apply an expression of type", quote (pThing ty)])
    PartiallyAppliedFunction info currExpr expected got ->
        combineTc
            info
            currExpr
            ( unwords
                ["Missing argument to function application, got", show got <> ",", "but expected", show expected]
            )
    TooManyArguments info currExpr expected got ->
        combineTc
            info
            currExpr
            ( unwords
                ["Too many arguments to function application, got", show got <> ",", "but expected", show expected]
            )
    AssignNonVariable info currExpr name ->
        combineTc
            info
            currExpr
            (unwords ["Can not assign an expression to the non-variable", quote (pThing name)])
    ExpectingImmutable info currExpr ->
        combineTc
            info
            currExpr
            (unwords ["Expecting an immutable variable, but got a mutable one"])
    TypeMustBeKnown info currExpr name ->
        combineTc
            info
            currExpr
            (unwords ["Type for", quote (pThing name), "must be known at this point"])
    ExpectedTyGotLambda info currExpr ty ->
        combineTc
            info
            currExpr
            (unwords ["Expected type", quote (pThing ty), "but got a lambda expression"])
    ExpectedLambdaNArgs info currExpr expected got ->
        combineTc
            info
            currExpr
            ( unwords
                ["Expected a lambda that takes", quote (show expected), "arguments, but it takes", quote (show got)]
            )
    ExpectedPatNArgs info currExpr pat expected got ->
        combineTc
            info
            currExpr
            ( unwords
                [ "The pattern"
                , quote (pThing pat)
                , "has"
                , show got
                , "matched fields,"
                , "but the constructor expects"
                , show expected
                , "matched fields"
                ]
            )

combineRn :: SourceInfo -> Text -> Text
combineRn info msg = mconcat [reportSourceInfo info, ":\n", indent 2 ("* " <> msg)]

combineTc :: SourceInfo -> [ExprRn] -> Text -> Text
combineTc info expr msg =
    let lastExpr = viaNonEmpty last expr
        line = Nothing -- fmap (subtract 1 . unPos . fst . start) info.spanInfo
     in mconcat
            [ "\n"
            , reportSourceInfo info
            , ":\n"
            , indent 4 msg
            , "\n"
            , case lastExpr of
                Nothing -> ""
                Just expr ->
                    indent
                        4
                        ("\n" <> Text.unlines (barAndLine line $ Text.lines (pThing expr)))
            ]

barAndLine :: Maybe Int -> [Text] -> [Text]
barAndLine n ys = go n ys
  where
    go _ [] = []
    go Nothing (x : xs) = "  |  " <> x : go Nothing xs
    go (Just line) (x : xs) =
        "  "
            <> show line
            <> Text.replicate (maxIndent n ys - length (show @String line) + 1) " "
            <> "|  "
            <> x
            : go (Just $ line + 1) xs
      where
        maxIndent Nothing _ = 0
        maxIndent (Just line) xs = length $ show @String $ line + length xs

reportSourceInfo :: SourceInfo -> Text
reportSourceInfo info = do
    let path = pack info.sourceFile
    let pos = do
            let (Span (startRow, startCol) (_endRow, _endCol)) = info.spanInfo
            pure $ mconcat [show $ unPos startRow, ":", show $ unPos startCol]
    mconcat [path, ":", fromMaybe "?:?" pos]

tyExpectedGot' ::
    (MonadReader Ctx m, MonadValidate [TcError] m) =>
    SourceInfo ->
    [TypeTc] ->
    TypeTc ->
    m a
tyExpectedGot' a c d = do
    exprStack <- view exprStack
    refute (return $ TyExpectedGot a exprStack c d)

tyExpectedGot ::
    (MonadReader Ctx m, MonadValidate [TcError] m) =>
    SourceInfo ->
    [TypeTc] ->
    TypeTc ->
    m ()
tyExpectedGot a c d = do
    names <- view names
    exprStack <- fmap (renameBack names) <$> view exprStack
    dispute (return $ TyExpectedGot a exprStack c d)

immutableVariable' ::
    (MonadReader Ctx m, MonadValidate [TcError] m) => SourceInfo -> Ident -> m a
immutableVariable' a c = do
    names <- view names
    exprStack <- fmap (renameBack names) <$> view exprStack
    let name = getOriginalName' c names
    refute (return $ ImmutableVariable a exprStack name)

immutableVariable ::
    (MonadReader Ctx m, MonadValidate [TcError] m) => SourceInfo -> Ident -> m ()
immutableVariable a c = do
    names <- view names
    exprStack <- fmap (renameBack names) <$> view exprStack
    let name = getOriginalName' c names
    dispute (return $ ImmutableVariable a exprStack name)

emptyReturnNonUnit' ::
    (MonadReader Ctx m, MonadValidate [TcError] m) => SourceInfo -> TypeTc -> m a
emptyReturnNonUnit' a c = do
    names <- view names
    exprStack <- fmap (renameBack names) <$> view exprStack
    refute (return $ EmptyReturnNonUnit a exprStack c)

emptyReturnNonUnit ::
    (MonadReader Ctx m, MonadValidate [TcError] m) => SourceInfo -> TypeTc -> m ()
emptyReturnNonUnit a c = do
    names <- view names
    exprStack <- fmap (renameBack names) <$> view exprStack
    dispute (return $ EmptyReturnNonUnit a exprStack c)

applyNonFunction' ::
    (MonadReader Ctx m, MonadValidate [TcError] m) => SourceInfo -> TypeTc -> m a
applyNonFunction' a c = do
    names <- view names
    exprStack <- fmap (renameBack names) <$> view exprStack
    refute (return $ ApplyNonFunction a exprStack c)

applyNonFunction ::
    (MonadReader Ctx m, MonadValidate [TcError] m) => SourceInfo -> TypeTc -> m ()
applyNonFunction a c = do
    names <- view names
    exprStack <- fmap (renameBack names) <$> view exprStack
    dispute (return $ ApplyNonFunction a exprStack c)

partiallyAppliedFunction' ::
    (MonadReader Ctx m, MonadValidate [TcError] m) => SourceInfo -> Int -> Int -> m a
partiallyAppliedFunction' a c d = do
    names <- view names
    exprStack <- fmap (renameBack names) <$> view exprStack
    refute (return $ PartiallyAppliedFunction a exprStack c d)

partiallyAppliedFunction ::
    (MonadReader Ctx m, MonadValidate [TcError] m) => SourceInfo -> Int -> Int -> m ()
partiallyAppliedFunction a c d = do
    names <- view names
    exprStack <- fmap (renameBack names) <$> view exprStack
    dispute (return $ PartiallyAppliedFunction a exprStack c d)

tooManyArguments' ::
    (MonadReader Ctx m, MonadValidate [TcError] m) => SourceInfo -> Int -> Int -> m a
tooManyArguments' a c d = do
    names <- view names
    exprStack <- fmap (renameBack names) <$> view exprStack
    refute (return $ TooManyArguments a exprStack c d)

tooManyArguments ::
    (MonadReader Ctx m, MonadValidate [TcError] m) => SourceInfo -> Int -> Int -> m ()
tooManyArguments a c d = do
    names <- view names
    exprStack <- fmap (renameBack names) <$> view exprStack
    dispute (return $ TooManyArguments a exprStack c d)

assignNonVariable' ::
    (MonadReader Ctx m, MonadValidate [TcError] m) => SourceInfo -> Ident -> m a
assignNonVariable' a c = do
    names <- view names
    exprStack <- fmap (renameBack names) <$> view exprStack
    let name = getOriginalName' c names
    refute (return $ AssignNonVariable a exprStack name)

assignNonVariable ::
    (MonadReader Ctx m, MonadValidate [TcError] m) => SourceInfo -> Ident -> m ()
assignNonVariable a c = do
    names <- view names
    exprStack <- fmap (renameBack names) <$> view exprStack
    let name = getOriginalName' c names
    dispute (return $ AssignNonVariable a exprStack name)

expectingImmutable' ::
    (MonadReader Ctx m, MonadValidate [TcError] m) => SourceInfo -> m a
expectingImmutable' a = do
    names <- view names
    exprStack <- fmap (renameBack names) <$> view exprStack
    refute (return $ ExpectingImmutable a exprStack)

expectingImmutable ::
    (MonadReader Ctx m, MonadValidate [TcError] m) => SourceInfo -> m ()
expectingImmutable a = do
    names <- view names
    exprStack <- fmap (renameBack names) <$> view exprStack
    dispute (return $ ExpectingImmutable a exprStack)

typeMustBeKnown' ::
    (MonadReader Ctx m, MonadValidate [TcError] m) => SourceInfo -> Ident -> m a
typeMustBeKnown' a c = do
    names <- view names
    exprStack <- fmap (renameBack names) <$> view exprStack
    let name = getOriginalName' c names
    refute (return $ TypeMustBeKnown a exprStack name)

typeMustBeKnown ::
    (MonadReader Ctx m, MonadValidate [TcError] m) => SourceInfo -> Ident -> m ()
typeMustBeKnown a c = do
    names <- view names
    exprStack <- fmap (renameBack names) <$> view exprStack
    let name = getOriginalName' c names
    dispute (return $ TypeMustBeKnown a exprStack name)

expectedTyGotLambda' ::
    (MonadReader Ctx m, MonadValidate [TcError] m) => SourceInfo -> TypeTc -> m a
expectedTyGotLambda' a c = do
    names <- view names
    exprStack <- fmap (renameBack names) <$> view exprStack
    refute (return $ ExpectedTyGotLambda a exprStack c)

expectedTyGotLambda ::
    (MonadReader Ctx m, MonadValidate [TcError] m) => SourceInfo -> TypeTc -> m ()
expectedTyGotLambda a c = do
    names <- view names
    exprStack <- fmap (renameBack names) <$> view exprStack
    dispute (return $ ExpectedTyGotLambda a exprStack c)

expectedLambdaNArgs' ::
    (MonadReader Ctx m, MonadValidate [TcError] m) => SourceInfo -> Int -> Int -> m a
expectedLambdaNArgs' a c d = do
    names <- view names
    exprStack <- fmap (renameBack names) <$> view exprStack
    refute (return $ ExpectedLambdaNArgs a exprStack c d)

expectedLambdaNArgs ::
    (MonadReader Ctx m, MonadValidate [TcError] m) => SourceInfo -> Int -> Int -> m ()
expectedLambdaNArgs a c d = do
    names <- view names
    exprStack <- fmap (renameBack names) <$> view exprStack
    dispute (return $ ExpectedLambdaNArgs a exprStack c d)

expectedPatNArgs ::
    (MonadReader Ctx m, MonadValidate [TcError] m) => SourceInfo -> PatternRn -> Int -> Int -> m ()
expectedPatNArgs loc pat expected got = do
    names <- view names
    exprStack <- fmap (renameBack names) <$> view exprStack
    dispute (return $ ExpectedPatNArgs loc exprStack pat expected got)

expectedPatNArgs' ::
    (MonadReader Ctx m, MonadValidate [TcError] m) => SourceInfo -> PatternRn -> Int -> Int -> m a
expectedPatNArgs' loc pat expected got = do
    names <- view names
    exprStack <- fmap (renameBack names) <$> view exprStack
    refute (return $ ExpectedPatNArgs loc exprStack pat expected got)

importDoesNotExist' :: (MonadValidate [RnError] m) => SourceInfo -> Namespace -> m a
importDoesNotExist' loc = refute . return . UnboundImport loc

importDoesNotExist :: (MonadValidate [RnError] m) => SourceInfo -> Namespace -> m ()
importDoesNotExist loc = dispute . return . UnboundImport loc

$(gen All "RnError")
$(gen All "ChError")
