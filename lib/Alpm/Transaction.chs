{-# LANGUAGE ForeignFunctionInterface, GeneralizedNewtypeDeriving #-}

{# context lib="alpm" prefix="alpm" #}

module Alpm.Transaction where

import Control.Applicative
import Control.Monad
import Control.Monad.Reader
import Control.Monad.Trans
import Data.Bits
import Foreign.C
import Foreign.Ptr
import Foreign.Marshal.Utils (toBool, fromBool)

import Alpm.Core
import Alpm.Database
import Alpm.Internal.Types
import Alpm.Utils

{# import Alpm.Internal.Types #}

#include <alpm.h>

newtype Transaction a = Transaction { transaction :: Alpm a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader AlpmEnv)

withTransaction :: Transaction a -> Alpm a
withTransaction trans = do
    withHandle $ flip {# call trans_init #} (1 `shiftL` 6)
    rst <- transaction trans
    withHandle $ {# call trans_release #}
    return rst

----------------------------------------------------------------------

data UpdateStatus = Updated
                  | UpToDate
                  deriving (Eq, Show)

updateDB :: Bool -> Database -> Transaction UpdateStatus
updateDB force db = Transaction $ do
    rst <- liftIO $ fromIntegral <$> {# call db_update #} (fromBool $ not force) db
    case rst of
        n | n  < 0    -> throwAlpmException "unable to update database"
          | n == 0    -> return UpToDate
          | otherwise -> return Updated
