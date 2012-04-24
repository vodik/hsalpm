{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls, MultiParamTypeClasses, TypeSynonymInstances, GeneralizedNewtypeDeriving #-}

module Alpm.Base where

import Control.Applicative
import Control.Monad
import Control.Monad.Error
import Control.Monad.Reader
import System.IO.Unsafe

import Foreign.C
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.ForeignPtr

import Alpm.Util

data AlpmHandle
data AlpmSession = AlpmSession !(ForeignPtr AlpmHandle)

newtype Alpm a = Alpm (ErrorT AlpmException (ReaderT AlpmSession IO) a)
    deriving (Functor, Applicative, Monad, MonadIO, MonadError AlpmException, MonadReader AlpmSession)

data AlpmException = AlpmException String String
                   | UnknownException
    deriving (Eq, Read, Show)

instance Error AlpmException where
    noMsg    = UnknownException
    strMsg _ = UnknownException

newtype Transaction a = Transaction (Alpm a)
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader AlpmSession)

data AlpmOptions = AlpmOptions
    { root   :: String
    , dbPath :: String
    }

class AlpmType a t where
    pack   :: a -> Ptr t
    unpack :: Ptr t -> a

instance AlpmType String CChar where
    pack   = unsafePerformIO . newCString
    unpack = unsafePerformIO . peekCString

withAlpmPtr :: (Ptr AlpmHandle -> IO b) -> Alpm b
withAlpmPtr f = ask >>= \(AlpmSession a) -> liftIO $ withForeignPtr a f

unsafeAlpmPtrToPtr :: Alpm (Ptr AlpmHandle)
unsafeAlpmPtrToPtr = ask >>= \(AlpmSession a) -> return $ unsafeForeignPtrToPtr a

foreign import ccall "alpm_strerror" c_alpm_strerror :: CInt -> CString
alpmStrerror errno = unsafePeekCString $ c_alpm_strerror errno

foreign import ccall "alpm_errno" c_alpm_errno :: Ptr AlpmHandle -> CInt
alpmLastStrerror = withAlpmPtr $
    return . unsafePeekCString . c_alpm_strerror . c_alpm_errno

throwAlpmException str = throwError =<< AlpmException str <$> alpmLastStrerror