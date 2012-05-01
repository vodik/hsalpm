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
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Storable
import System.IO.Unsafe

import System.Alpm.Internal.Types
import System.Alpm.StringLike
import System.Alpm.Utils

{# import System.Alpm.Internal.Types #}

#include <alpm.h>

type AlpmHandle = ForeignPtr ()

foreign import ccall "&alpm_release" c_alpm_release :: FinalizerPtr ()

alpmInitialize :: FilePath -> FilePath -> IO (Either ErrorCode AlpmHandle)
alpmInitialize root dbPath = do
    root'   <- newCString root
    dbPath' <- newCString dbPath
    alloca $ \errPtr -> do
        alpm_ptr <- {# call initialize #} root' dbPath' errPtr
        if alpm_ptr == nullPtr
            then Left  <$> errno (castPtr errPtr)
            else Right <$> newForeignPtr c_alpm_release alpm_ptr

strerror :: StringLike s => ErrorCode -> s
strerror err = unsafePerformIO $ do
    {# call strerror #} (fromIntegral $ fromEnum err) >>= fromC

-- errno :: Ptr a -> ErrorCode
errno e = {# call errno #} e >>= return . toEnum . fromIntegral

alpmVersion :: StringLike a => IO a
alpmVersion = readString {# call version #}

alpmCapabilities :: IO [Capabilities]
alpmCapabilities = fromBitfield <$> {# call capabilities #}
