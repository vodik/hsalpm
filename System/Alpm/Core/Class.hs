{-# LANGUAGE FlexibleContexts #-}

module System.Alpm.Core.Class where

import Control.Monad.Error
import System.Alpm.Core.Error

class (Monad m, MonadError AlpmError m) => MonadAlpm m where
    test :: m ()
