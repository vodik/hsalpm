{-# LANGUAGE ForeignFunctionInterface #-}

module Alpm.Option where

import Control.Applicative
import Control.DeepSeq
import Control.Monad.Reader
import Data.Word
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
    wrap_cb_progress :: CBProgress -> IO (FunPtr CBProgress)
foreign import ccall "wrapper"
    wrap_cb_log :: CBLog -> IO (FunPtr CBLog)

foreign import ccall "alpm_option_set_progresscb"
    c_alpm_option_set_progresscb :: Ptr AlpmHandle -> FunPtr CBProgress -> IO CInt
foreign import ccall "alpm_option_set_logcb"
    c_alpm_option_set_logcb :: Ptr AlpmHandle -> FunPtr CBLog -> IO CInt

setProgressCB :: (Int -> String -> Int -> Word -> Word -> IO ()) -> Alpm ()
setProgressCB f = withAlpmPtr $ \alpm_ptr -> do
    cbW <- wrap_cb_progress $ \a b c d e -> do
        let a' = fromIntegral a
            c' = fromIntegral c
            d' = fromIntegral d
            e' = fromIntegral e
        b' <- peekCString b
        f a' b' c' d' e'
    c_alpm_option_set_progresscb alpm_ptr cbW
    -- freeHaskellFunPtr cbW
    return ()

setLogCB :: (Int -> String -> IO ()) -> Alpm ()
setLogCB f = withAlpmPtr $ \alpm_ptr -> do
    cbW <- wrap_cb_log $ \lvl str ->
        peekCString str >>= f (fromIntegral lvl)
    c_alpm_option_set_logcb alpm_ptr cbW
    -- freeHaskellFunPtr cbW
    return ()
