{-# LANGUAGE ForeignFunctionInterface #-}

module Alpm.Option where

import Control.Applicative
import Control.DeepSeq
import Control.Monad.Reader
import Text.Printf

import Foreign.C
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Storable

import Alpm.Base
import Alpm.Database
import Alpm.Network
import Alpm.Package
import Alpm.Util

type CBProgress = CInt -> CString -> CInt -> CSize -> CSize -> IO ()
type CBLog = CInt -> CString -> IO ()

foreign import ccall "wrapper"
    cb_progress :: CBProgress -> IO (FunPtr CBProgress)
foreign import ccall "wrapper"
    cb_log :: CBLog -> IO (FunPtr CBLog)

foreign import ccall "alpm_option_set_progresscb"
    c_alpm_option_set_progresscb :: Ptr AlpmHandle -> FunPtr CBProgress -> IO CInt
foreign import ccall "alpm_option_set_logcb"
    c_alpm_option_set_logcb :: Ptr AlpmHandle -> FunPtr CBLog -> IO CInt

setProgressCB f = withAlpmPtr $ \alpm_ptr -> do
    cbW <- cb_progress f
    _ <- c_alpm_option_set_progresscb alpm_ptr cbW
    -- freeHaskellFunPtr cbW
    return ()

setLogCB f = withAlpmPtr $ \alpm_ptr -> do
    cbW <- cb_log f
    _ <- c_alpm_option_set_logcb alpm_ptr cbW
    -- freeHaskellFunPtr cbW
    return ()
