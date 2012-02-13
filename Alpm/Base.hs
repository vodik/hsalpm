{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls, GeneralizedNewtypeDeriving #-}

module Alpm.Base where

import Control.Applicative
import Control.Monad
import Control.Monad.Reader

import Foreign.C
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.ForeignPtr

import Alpm.Util

data AlpmHandle
data AlpmSession = AlpmSession !(ForeignPtr AlpmHandle)

newtype Alpm a = Alpm (ReaderT AlpmSession IO a)
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader AlpmSession)

data AlpmOptions = AlpmOptions
    { root      :: String
    , dbPath    :: String
    , cachePath :: Maybe String
    }

foreign import ccall "alpm_option_add_cachedir" alpm_option_add_cachedir :: Ptr AlpmHandle -> CString -> IO ()
setAlpmOptions :: (Ptr AlpmHandle -> CString -> IO ()) -> Ptr AlpmHandle -> String -> IO ()
setAlpmOptions f h v = newCString v >>= f h

withAlpmPtr :: (Ptr AlpmHandle -> IO b) -> Alpm b
withAlpmPtr f = ask >>= \(AlpmSession a) -> liftIO $ withForeignPtr a f

foreign import ccall "alpm_strerror" c_alpm_strerror :: CInt -> CString
alpmStrerror errno = unsafePeekCString $ c_alpm_strerror errno

foreign import ccall "alpm_errno" c_alpm_errno :: Ptr AlpmHandle -> CInt
alpmLastStrerror = withAlpmPtr $
    return . unsafePeekCString . c_alpm_strerror . c_alpm_errno
