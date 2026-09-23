{-# LANGUAGE TemplateHaskell #-}

module Frontend.Typechecker.Ctx where

import Control.Lens (makeLenses)
import Control.Lens.Setter (locally)
import Control.Monad.Reader (MonadReader)
import Frontend.Renamer.Types (ExprRn, FnRn)
import Frontend.Typechecker.Types (TypeTc, Tc)
import Frontend.Types (SourceInfo)
import Names (Names)
import Relude (Show)
import Table (DefTable)

data Ctx = Ctx
    { _defTable :: DefTable Tc TypeTc SourceInfo
    , _returnType :: TypeTc
    , _currentFun :: FnRn
    , _exprStack :: [ExprRn]
    , _names :: Names
    }
    deriving (Show)

$(makeLenses ''Ctx)

push :: (MonadReader Ctx m) => ExprRn -> m a -> m a
push expr = locally exprStack (expr :)
