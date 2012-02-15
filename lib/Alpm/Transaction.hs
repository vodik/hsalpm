{-# LANGUAGE ForeignFunctionInterface #-}

module Alpm.Transaction where

import Data.Bits
import Control.Monad.Trans (liftIO)

import Alpm
import Alpm.Base

import Foreign.C
import Foreign.Ptr (Ptr, nullPtr)

foreign import ccall "alpm_trans_init"    c_alpm_trans_init    :: Ptr AlpmHandle -> CInt -> IO CInt
foreign import ccall "alpm_trans_release" c_alpm_trans_release :: Ptr AlpmHandle -> IO CInt

withTransaction :: Alpm a -> Alpm a
withTransaction f = do
    alpm_ptr <- unsafeAlpmPtrToPtr
    liftIO $ c_alpm_trans_init alpm_ptr (1 `shiftL` 6)
    rst <- f
    liftIO $ c_alpm_trans_release alpm_ptr
    return rst
