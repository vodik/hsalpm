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
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader AlpmHandle)

withTransaction :: [TransactionFlags] -> Transaction a -> Alpm a
withTransaction flags trans = do
    withHandle $ flip {# call trans_init #} (toBitfield flags)
    rst <- transaction trans
    withHandle $ {# call trans_release #}
    return rst

----------------------------------------------------------------------

-- loadPkg filename full level = do
    -- alloca $ \pkg -> do
        -- rst <- withHandle $ \h -> {# call pkg_load #} h filename full (toBitfield level) pkg

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

-- TODO: replace void with error handling
syncSysupgrade :: Bool -> Transaction ()
syncSysupgrade downgrade = Transaction $ do
    void . withHandle $ flip {# call sync_sysupgrade #} (fromBool downgrade)
