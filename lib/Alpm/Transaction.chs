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
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils (toBool, fromBool)
import Foreign.Storable

import System.IO.Unsafe

import Alpm.Core
import Alpm.Caching
import Alpm.Database
import Alpm.Internal.Types
import Alpm.StringLike
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

instance Storable Package where
    sizeOf (Package r)    = sizeOf r
    alignment (Package r) = alignment r

    peek p             = Package <$> peek (castPtr p)
    poke p (Package r) = poke (castPtr p) r

loadPkg :: (StringLike s) => s -> Bool -> [SignatureLevel] -> Alpm Package
loadPkg filename full level = do
    rst <- withHandle $ \h -> do
        pkg <- malloc
        fn  <- toC filename
        rst <- {# call pkg_load #} h fn (fromBool full) (toBitfield level) pkg
        if rst < 0
            then return Nothing
            else return . Just $ Package pkg
    case rst of
        Nothing -> throwAlpmException "unable to load package"
        Just p  -> return p

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
