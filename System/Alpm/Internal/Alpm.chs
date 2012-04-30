{-# LANGUAGE ForeignFunctionInterface #-}

module System.Alpm.Internal.Alpm
    ( AlpmHandle
    , alpmInitialize
    , strerror, errno
    , alpmVersion
    , alpmCapabilities
    ) where

{# context lib="alpm" prefix="alpm" #}

import Control.Applicative
import Foreign.C
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Storable

import System.Alpm.Internal.Types
import System.Alpm.StringLike
import System.Alpm.Utils

#include <alpm.h>

type AlpmHandle = ForeignPtr ()

foreign import ccall "&alpm_release" c_alpm_release :: FinalizerPtr ()

alpmInitialize :: FilePath -> FilePath -> IO (Either String AlpmHandle)
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

alpmVersion :: StringLike a => IO a
alpmVersion = readString {# call version #}

alpmCapabilities :: IO [Capabilities]
alpmCapabilities = fromBitmap <$> {# call capabilities #}
