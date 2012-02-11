{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls, GeneralizedNewtypeDeriving #-}

module Alpm where

import Control.Applicative
import Control.Exception
import Control.DeepSeq
import Control.Monad
import Control.Monad.Reader
import Text.Printf

import Foreign.C
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Storable

import Alpm.List
import Alpm.Package
import Alpm.Util

data AlpmHandle

data AlpmConf = AlpmConf
    { alpmPtr :: !(ForeignPtr AlpmHandle)
    , options :: AlpmOptions
    }

data DB = DB !(Ptr AlpmList)

newtype Alpm a = Alpm (ReaderT AlpmConf IO a)
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader AlpmConf)

data AlpmOptions = AlpmOptions
    { root      :: String
    , dbPath    :: String
    , cachePath :: Maybe String
    }

runAlpm :: (NFData a) => AlpmOptions -> Alpm a -> IO a
runAlpm opt (Alpm f) =
    alpmInitialize opt >>= \alpm -> withForeignPtr (alpmPtr alpm) $ \alpm_ptr -> do
        whenJust (cachePath opt) $ setAlpmOptions alpm_option_add_cachedir alpm_ptr
        r <- runReaderT f alpm
        return $!! r

defaultOptions = AlpmOptions
    { root      = "/"
    , dbPath    = "/var/lib/pacman"
    , cachePath = Nothing
    }

foreign import ccall "alpm_initialize" c_alpm_initialize :: CString -> CString -> Ptr CInt -> IO (Ptr AlpmHandle)
alpmInitialize :: AlpmOptions -> IO AlpmConf
alpmInitialize opt = do
    root'   <- newCString $ root opt
    dbPath' <- newCString $ dbPath opt
    alloca $ \errPtr -> do
        alpm_ptr <- c_alpm_initialize root' dbPath' errPtr
        if isNull alpm_ptr
            then peek errPtr >>= fail . printf "failed to initialize alpm library (%s)" . alpmStrerror
            else flip AlpmConf opt <$> newForeignPtr c_alpm_release alpm_ptr

foreign import ccall "&alpm_release" c_alpm_release :: FinalizerPtr AlpmHandle

foreign import ccall "alpm_option_add_cachedir" alpm_option_add_cachedir :: Ptr AlpmHandle -> CString -> IO ()
setAlpmOptions :: (Ptr AlpmHandle -> CString -> IO ()) -> Ptr AlpmHandle -> String -> IO ()
setAlpmOptions f h v = newCString v >>= f h

withAlpmPtr :: (Ptr AlpmHandle -> IO b) -> Alpm b
withAlpmPtr f = asks alpmPtr >>= \a -> liftIO $ withForeignPtr a f

foreign import ccall "alpm_option_get_localdb" c_alpm_option_get_localdb :: Ptr AlpmHandle -> IO (Ptr AlpmList)
localDB :: Alpm DB
localDB = withAlpmPtr $ \alpm_ptr -> do
    db_ptr <- c_alpm_option_get_localdb alpm_ptr
    if isNull db_ptr
        then fail "could not register 'local' database"
        else return $ DB db_ptr

foreign import ccall "alpm_db_get_pkgcache" c_alpm_db_get_pkgcache :: Ptr AlpmList -> Ptr AlpmList
packages :: DB -> [Package]
packages (DB db_ptr) = integrate (mkPackage . c_alpm_list_getdata) $ c_alpm_db_get_pkgcache db_ptr
