{-# LANGUAGE ForeignFunctionInterface, GeneralizedNewtypeDeriving #-}

{# context lib="alpm" prefix="alpm" #}

module System.Alpm.Transaction where

import Control.Applicative
import Control.Exception
import Control.Monad
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.Trans
import Data.Bits
import Foreign.C
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils (toBool, fromBool)
import Foreign.Storable

import System.Alpm.Core
import System.Alpm.Cache
import System.Alpm.Database
import System.Alpm.Internal.Alpm
import System.Alpm.Internal.Types
import System.Alpm.StringLike
import System.Alpm.Utils

{# import System.Alpm.Internal.Types #}

#include <alpm.h>

newtype Transaction a = Transaction { runTransaction :: Alpm a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader AlpmHandle, MonadError AlpmError)

instance MonadStage Transaction where
    stage Add    = addPkg
    stage Remove = removePkg

withTransaction :: [TransactionFlags] -> Transaction a -> Alpm a
withTransaction flags trans = do
    initialize flags
    rst <- runTransaction trans
    run <- runErrorT $ prepare >> commit
    release
    case run of
        Left  e -> throwAlpmException e
        Right _ -> return rst
  where
    initialize flags = do
        rst <- withHandle $ flip {# call trans_init #} (toBitfield flags)
        when (rst == -1) $ throwAlpmException "failed to initialize transaction"

    prepare = ErrorT $ withHandle $ \h -> do
        alloca $ \ptr -> do
            rst <- {# call trans_prepare #} h ptr
            if rst /= -1
                then return $ Right ()
                else errno h >>= \err -> do
                    return . Left $ case err of
                        ErrPkgInvalidArch  -> undefined
                        ErrUnsatisfiedDeps -> undefined
                        ErrConflictingDeps -> undefined
                        _ -> "failed to prepare transaction"

    commit = ErrorT $ withHandle $ \h -> do
        alloca $ \ptr -> do
            rst <- {# call trans_commit #} h ptr
            if rst /= -1
                then return $ Right ()
                else errno h >>= \err -> do
                    return . Left $ case err of
                        ErrFileConflicts      -> undefined
                        ErrPkgInvalid         -> undefined
                        ErrPkgInvalidChecksum -> undefined
                        ErrPkgInvalidSig      -> undefined
                        ErrDltInvalid         -> undefined
                        _ -> "failed to commit transaction"

    release = do
        rst <- withHandle $ {# call trans_release #}
        when (rst == -1) $ throwAlpmException "failed to release transaction"

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
            else Just <$> peek pkg
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
    rst <- withHandle $ flip {# call sync_sysupgrade #} (fromBool downgrade)
    when (rst /= 0) $ throwAlpmException "failed to do sys upgrade"

-- TODO: replace void with error handling
addPkg :: Package -> Transaction ()
addPkg pkg = Transaction $ do
    rst <- withHandle $ flip {# call add_pkg #} pkg
    when (rst /= 0) $ throwAlpmException "failed to add package"

-- TODO: replace void with error handling
removePkg :: Package -> Transaction ()
removePkg pkg = Transaction $ do
    rst <- withHandle $ flip {# call remove_pkg #} pkg
    when (rst /= 0) $ throwAlpmException "failed to remove package"
