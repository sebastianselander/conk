{-# LANGUAGE TemplateHaskell #-}

module Frontend.Typechecker.Ctx where

import Control.Lens (makeLenses)
import Control.Lens.Setter (locally)
import Control.Monad.Reader (MonadReader)
import Relude (Show)

import Frontend.Renamer.Types (ExprRn, FnRn)
import Frontend.Typechecker.Types (PolyType, Tc, TypeTc)
import Frontend.Types (SourceInfo)
import Names (Names)
import Table (DefTable)


data Ctx = Ctx
    { _defTable :: DefTable Tc (PolyType Tc) SourceInfo
    , _returnType :: TypeTc
    , _currentFun :: FnRn
    , _exprStack :: [ExprRn]
    , _names :: Names
    }
    deriving (Show)


$(makeLenses ''Ctx)


push :: (MonadReader Ctx m) => ExprRn -> m a -> m a
push expr = locally exprStack (expr :)
