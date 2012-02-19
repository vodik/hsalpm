{-# LANGUAGE ExistentialQuantification, ForeignFunctionInterface #-}

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

data Attr a = Attr (Alpm a) (a -> Alpm ())

data AttrOp = forall a. Attr a := a
            | forall a. Attr a :~ (a -> a)

infixr 0 :=, :~

get :: Attr a -> Alpm a
get (Attr getter setter) = getter

set :: [AttrOp] -> Alpm ()
set = mapM_ app
  where
    app (Attr getter setter := x) = setter x
    app (Attr getter setter :~ f) = getter >>= \v -> setter (f v)

newAlpmStringAttr getter setter = Attr alpmGetter alpmSetter
  where
    alpmGetter   = withAlpmPtr $ \ptr -> do
        ret <- getter ptr
        if isNull ret
            then return ""
            else peekCString ret
    alpmSetter v = withAlpmPtr $ \ptr -> newCString v >>= setter ptr

--------------------------------------------------------------------------------

foreign import ccall "alpm_option_get_arch" c_alpm_option_get_arch :: Ptr AlpmHandle -> IO CString
foreign import ccall "alpm_option_set_arch" c_alpm_option_set_arch :: Ptr AlpmHandle -> CString -> IO ()
arch = newAlpmStringAttr c_alpm_option_get_arch c_alpm_option_set_arch

foreign import ccall "alpm_option_get_logfile" c_alpm_option_get_logfile :: Ptr AlpmHandle -> IO CString
foreign import ccall "alpm_option_set_logfile" c_alpm_option_set_logfile :: Ptr AlpmHandle -> CString -> IO ()
logFile = newAlpmStringAttr c_alpm_option_get_logfile c_alpm_option_set_logfile

foreign import ccall "alpm_option_get_gpgdir" c_alpm_option_get_gpgdir :: Ptr AlpmHandle -> IO CString
foreign import ccall "alpm_option_set_gpgdir" c_alpm_option_set_gpgdir :: Ptr AlpmHandle -> CString -> IO ()
gpgDirectory = newAlpmStringAttr c_alpm_option_get_gpgdir c_alpm_option_set_gpgdir

--------------------------------------------------------------------------------

foreign import ccall "alpm_option_add_cachedir" c_alpm_option_add_cachedir :: Ptr AlpmHandle -> CString -> IO ()
setAlpmOptions :: (Ptr AlpmHandle -> CString -> IO ()) -> Ptr AlpmHandle -> String -> IO ()
setAlpmOptions f h v = newCString v >>= f h

--------------------------------------------------------------------------------

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
