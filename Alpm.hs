{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls, GeneralizedNewtypeDeriving #-}

module Alpm where

import Control.Applicative
import Control.Exception
import Control.Monad
import Control.Monad.Reader
import System.IO.Unsafe

import Foreign.C
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc

import Alpm.Package

data AlpmHandle
data AlpmList

data AlpmConf = AlpmConf
    { alpmPtr :: !(ForeignPtr AlpmHandle)
    , options :: AlpmOptions
    }

data DB = DB !(Ptr AlpmList)

newtype Alpm a = Alpm (ReaderT AlpmConf IO a)
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader AlpmConf)

data AlpmOptions = AlpmOptions
    { root   :: String
    , dbPath :: String
    }

foreign import ccall "alpm_initialize" c_alpm_initialize :: CString -> CString -> Ptr a -> IO (Ptr b)
alpmInitialize :: AlpmOptions -> IO AlpmConf
alpmInitialize opt = do
    root'    <- newCString $ root opt
    dbpath'  <- newCString $ dbPath opt
    alpm_ptr <- c_alpm_initialize root' dbpath' nullPtr
    if alpm_ptr == nullPtr
        then fail "alpm_initialize returned null"
        else flip AlpmConf opt <$> newForeignPtr c_alpm_release alpm_ptr

foreign import ccall "&alpm_release" c_alpm_release :: FinalizerPtr a

withAlpmPtr :: (Ptr AlpmHandle -> IO b) -> Alpm b
withAlpmPtr f = asks alpmPtr >>= \a -> liftIO $ withForeignPtr a f

foreign import ccall "alpm_option_get_localdb" c_alpm_option_get_localdb :: Ptr a -> IO (Ptr b)
localDB :: Alpm DB
localDB = withAlpmPtr $ \alpm_ptr -> do
    db_ptr <- c_alpm_option_get_localdb alpm_ptr
    if db_ptr == nullPtr
        then fail "could not register 'local' database"
        else return $ DB db_ptr

foreign import ccall "alpm_db_get_pkgcache" c_alpm_db_get_pkgcache :: Ptr a -> IO (Ptr b)
packages :: DB -> [Package]
packages (DB db_ptr) = unsafePerformIO $ do
    cache_ptr <- c_alpm_db_get_pkgcache db_ptr
    if cache_ptr == nullPtr
        then fail "could not get package cache"
        else return $ packages' cache_ptr

foreign import ccall "alpm_list_next"    c_alpm_list_next    :: Ptr a -> IO (Ptr b)
foreign import ccall "alpm_list_getdata" c_alpm_list_getdata :: Ptr a -> IO (Ptr b)
packages' :: Ptr a -> [Package]
packages' ptr
    | ptr == nullPtr = []
    | otherwise      = let next = unsafePerformIO $ c_alpm_list_next ptr
                       in boxPackage ptr : packages' next
  where
    boxPackage ptr = Package . unsafePerformIO $ c_alpm_list_getdata ptr

withAlpm :: AlpmOptions -> Alpm a -> IO a
withAlpm opt (Alpm f) = do
    a <- alpmInitialize opt
    runReaderT f a

defaultOptions = AlpmOptions
    { root   = "/"
    , dbPath = "/var/lib/pacman"
    }
