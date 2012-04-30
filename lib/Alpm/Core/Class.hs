module Alpm.Core.Class where

import Alpm.Core.Error

class (Monad m, MonadError AlpmError m) => MonadAlpm a where
    test :: m ()
