{-# LANGUAGE ForeignFunctionInterface #-}

module Alpm where

import Control.Applicative
import Control.DeepSeq
import Control.Monad.Reader
import Text.Printf

import Foreign.C
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Storable

import Alpm.Base
import Alpm.Database
import Alpm.Network
import Alpm.Option
import Alpm.Package
import Alpm.Util

runAlpm :: AlpmOptions -> Alpm a -> IO a
runAlpm opt (Alpm f) =
    alpmInitialize opt >>= \alpm@(AlpmSession a) ->
        withForeignPtr a $ \_ -> runReaderT f alpm

defaultOptions = AlpmOptions
    { root   = "/"
    , dbPath = "/var/lib/pacman"
    }

foreign import ccall "alpm_initialize" c_alpm_initialize :: CString -> CString -> Ptr CInt -> IO (Ptr AlpmHandle)
alpmInitialize :: AlpmOptions -> IO AlpmSession
alpmInitialize opt = do
    root'   <- newCString $ root opt
    dbPath' <- newCString $ dbPath opt
    alloca $ \errPtr -> do
        alpm_ptr <- c_alpm_initialize root' dbPath' errPtr
        if isNull alpm_ptr
            then peek errPtr >>= fail . printf "failed to initialize alpm library (%s)" . alpmStrerror
            else AlpmSession <$> newForeignPtr c_alpm_release alpm_ptr

foreign import ccall "&alpm_release" c_alpm_release :: FinalizerPtr AlpmHandle
