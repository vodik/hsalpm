{-# LANGUAGE ForeignFunctionInterface #-}

module Alpm.Internal.Alpm
    ( Handle
    , alpmInitialize
    , strerror, errno
    ) where

{# context lib="alpm" prefix="alpm" #}

import Control.Applicative
import Foreign.C
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Storable

import Alpm.Internal.Types

#include <alpm.h>

type Handle = ForeignPtr ()

foreign import ccall "&alpm_release" c_alpm_release :: FinalizerPtr ()

alpmInitialize :: FilePath -> FilePath -> IO (Either String Handle)
alpmInitialize root dbPath = do
    root'   <- newCString root
    dbPath' <- newCString dbPath
    alloca $ \errPtr -> do
        alpm_ptr <- {# call initialize #} root' dbPath' errPtr
        if alpm_ptr == nullPtr
            then Left  <$> (peek errPtr >>= strerror)
            else Right <$> newForeignPtr c_alpm_release alpm_ptr

strerror :: CInt -> IO String
strerror err = {# call strerror #} err >>= peekCString

errno :: Ptr () -> IO CInt
errno = {# call errno #}
