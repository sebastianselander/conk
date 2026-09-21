{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TemplateHaskell #-}

module Frontend.StatementCheck (check) where

import Control.Lens
import Control.Monad.Validate (MonadValidate, Validate, runValidate)
import Frontend.Error
import Frontend.Renamer.Types
import Frontend.Types
import Relude

newtype Ctx = Ctx {_inLoop :: Bool}
    deriving (Show)

$(makeLenses ''Ctx)

newtype ChM a = ChM {runCh :: ReaderT Ctx (Validate [ChError]) a}
    deriving (Functor, Applicative, Monad, MonadReader Ctx, MonadValidate [ChError])

runCheck :: Ctx -> ChM a -> Either [ChError] a
runCheck ctx = runValidate . flip runReaderT ctx . runCh

check :: ProgramRn -> Either [ChError] ProgramRn
check prg@(Program _ defs) = case lefts $ map checkDef defs of
    [] -> pure prg
    xs -> Left $ concat xs

checkFunction :: FnRn -> Either [ChError] ()
checkFunction (Fn info name tyParams _ returnType block) = runCheck (Ctx False) $ case returnType of
    TyLit NoExtField Unit -> breakBlock block
    _ -> case block of
        Block _ _ (Just _) -> breakBlock block
        _ -> do
            breakBlock block
            returnBlock block >>= \case
                True -> pure ()
                False -> missingReturn info name

checkDef :: DefRn -> Either [ChError] ()
checkDef (DefFn fn) = checkFunction fn
checkDef (DefAdt _) = pure ()
checkDef (DefImport _) = pure ()

breakBlock :: BlockRn -> ChM ()
breakBlock (Block _ statements tail) = mapM_ breakStmt statements >> mapM_ breakExpr tail

breakStmt :: StmtRn -> ChM ()
breakStmt = \case
    SExpr NoExtField expr -> breakExpr expr

breakExpr :: ExprRn -> ChM ()
breakExpr = \case
    Lit _ _ -> pure ()
    Var _ _ -> pure ()
    Prefix _ _ r -> breakExpr r
    BinOp _ l _ r -> breakExpr l >> breakExpr r
    App _ l rs -> breakExpr l >> mapM_ breakExpr rs
    Let _ _ expr -> breakExpr expr
    Ass _ _ _ expr -> breakExpr expr
    Ret _ _ -> pure ()
    EBlock NoExtField block -> breakBlock block
    Break info _ -> do
        loop <- view inLoop
        if loop
            then pure ()
            else do
                breakOutsideLoop info
    If _ expr true false -> do
        breakExpr expr
        breakBlock true
        mapM_ breakBlock false
    While _ expr block -> breakExpr expr >> locally inLoop (const True) (breakBlock block)
    Loop _ block -> locally inLoop (const True) (breakBlock block)
    Lam _ _ body -> breakExpr body
    Match _ scrutinee arms -> do
        breakExpr scrutinee
        mapM_ breakMatchArm arms

breakMatchArm :: MatchArmRn -> ChM ()
breakMatchArm (MatchArm _ _ expr) = breakExpr expr

returnBlock :: BlockRn -> ChM Bool
returnBlock (Block _ statements _) = returnStmts statements

returnStmts :: [StmtRn] -> ChM Bool
returnStmts [] = pure False
returnStmts (x : xs) = do
    returnStmt x >>= \case
        False -> returnStmts xs
        True -> case xs of
            [] -> pure True
            _ -> unreachableStatement (hasInfoStmt x) >> pure True

returnStmt :: StmtRn -> ChM Bool
returnStmt = \case
    SExpr NoExtField expr -> returnExpr expr

returnExpr :: ExprRn -> ChM Bool
returnExpr = \case
    Lit _ _ -> pure False
    Var _ _ -> pure False
    BinOp _ l _ r -> (||) <$> returnExpr l <*> returnExpr r
    Prefix _ _ expr -> returnExpr expr
    Ret _ _ -> pure True
    EBlock NoExtField block -> returnBlock block
    App _ l rs -> (||) <$> returnExpr l <*> anyM returnExpr rs
    Let _ _ expr -> returnExpr expr
    Ass _ _ _ expr -> returnExpr expr
    Break _ _ -> pure False
    If _ expr true false -> do
        if
            | alwaysTrue expr
            , Just (Block info _ _) <- false ->
                unreachableStatement info >> returnBlock true
            | alwaysTrue expr -> returnBlock true
            | alwaysFalse expr
            , Block info _ _ <- true ->
                unreachableStatement info >> maybe (pure False) returnBlock false
            | otherwise -> (&&) <$> returnBlock true <*> maybe (pure False) returnBlock false
    While _ expr block -> if alwaysTrue expr then returnBlock block else pure False
    Loop _ block -> returnBlock block
    Lam {} -> pure False
    Match _ scrutinee arms -> (&&) <$> returnExpr scrutinee <*> allM returnArm arms

returnArm :: MatchArmRn -> ChM Bool
returnArm (MatchArm _ _ body) = returnExpr body

-- TODO: Make mini evaluator
alwaysTrue :: ExprRn -> Bool
alwaysTrue = const False

-- TODO: Make mini evaluator
alwaysFalse :: ExprRn -> Bool
alwaysFalse = const False

hasInfoStmt :: StmtRn -> SourceInfo
hasInfoStmt = \case
    SExpr NoExtField expr -> hasInfoExpr expr

hasInfoExpr :: ExprRn -> SourceInfo
hasInfoExpr = \case
    Lit info _ -> info
    Var (info, _, _) _ -> info
    Prefix info _ _ -> info
    BinOp info _ _ _ -> info
    App info _ _ -> info
    Let (info, _) _ _ -> info
    Ass (info, _, _) _ _ _ -> info
    Ret info _ -> info
    EBlock NoExtField (Block info _ _) -> info
    Break info _ -> info
    If info _ _ _ -> info
    While info _ _ -> info
    Loop info _ -> info
    Lam info _ _ -> info
    Match info _ _ -> info
