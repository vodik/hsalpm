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

type CBFunc = CInt -> CString -> CInt -> CSize -> CSize -> IO ()

foreign import ccall "wrapper"
    cb_progress :: CBFunc
                -> IO (FunPtr CBFunc)

foreign import ccall "alpm_option_set_progresscb"
    c_alpm_option_set_progresscb :: Ptr AlpmHandle -> FunPtr CBFunc -> IO CInt

setProgressCB f = withAlpmPtr $ \alpm_ptr -> do
    cbW <- cb_progress f
    _ <- c_alpm_option_set_progresscb alpm_ptr cbW
    freeHaskellFunPtr cbW
