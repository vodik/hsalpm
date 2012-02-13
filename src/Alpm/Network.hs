{-# LANGUAGE ForeignFunctionInterface #-}

module Alpm.Network where

import Control.Applicative

import Foreign.C
import Foreign.Ptr (Ptr, nullPtr)

import Alpm.Base
import Alpm.Util

foreign import ccall "alpm_fetch_pkgurl" c_alpm_fetch_pkgurl :: Ptr AlpmHandle -> CString -> IO CString
fetchPkgUrl :: String -> Alpm (Maybe String)
fetchPkgUrl url = withAlpmPtr $ \alpm_ptr -> do
    url' <- newCString url
    ret  <- c_alpm_fetch_pkgurl alpm_ptr url'
    if isNull ret
        then return Nothing
        else Just <$> peekCString ret
