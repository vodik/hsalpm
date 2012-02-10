{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls, GeneralizedNewtypeDeriving #-}

module Alpm where

import Control.Applicative
import Control.Exception
import Control.Monad
import Control.Monad.Reader
import System.IO.Unsafe
import Text.Printf

import Foreign.C
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Storable

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

withAlpm :: AlpmOptions -> Alpm a -> IO a
withAlpm opt (Alpm f) = do
    a <- alpmInitialize opt
    runReaderT f a

defaultOptions = AlpmOptions
    { root   = "/"
    , dbPath = "/var/lib/pacman"
    }

foreign import ccall "alpm_strerror" c_alpm_strerror :: CInt -> CString
alpmStrerror errno = unsafePerformIO . peekCString $ c_alpm_strerror errno

foreign import ccall "alpm_initialize" c_alpm_initialize :: CString -> CString -> Ptr CInt -> IO (Ptr AlpmHandle)
alpmInitialize :: AlpmOptions -> IO AlpmConf
alpmInitialize opt = do
    root'   <- newCString $ root opt
    dbpath' <- newCString $ dbPath opt
    alloca $ \errPtr -> do
        alpm_ptr <- c_alpm_initialize root' dbpath' errPtr
        if isNull alpm_ptr
            then peek errPtr >>= fail . printf "failed to initialize alpm library (%s)" . alpmStrerror
            else flip AlpmConf opt <$> newForeignPtr c_alpm_release alpm_ptr

foreign import ccall "&alpm_release" c_alpm_release :: FinalizerPtr AlpmHandle

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
foreign import ccall "alpm_list_next"       c_alpm_list_next       :: Ptr AlpmList -> Ptr AlpmList
foreign import ccall "alpm_list_getdata"    c_alpm_list_getdata    :: Ptr AlpmList -> Ptr b

packages :: DB -> [Package]
packages (DB db_ptr) = integrate (Package . c_alpm_list_getdata) $ c_alpm_db_get_pkgcache db_ptr

integrate :: (Ptr AlpmList -> b) -> Ptr AlpmList -> [b]
integrate box ptr
    | isNull ptr = []
    | otherwise  = box ptr : integrate box (c_alpm_list_next ptr)

isNull :: Ptr a -> Bool
isNull = (== nullPtr)
