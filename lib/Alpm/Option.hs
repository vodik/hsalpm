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
import Alpm.List
import Alpm.Network
import Alpm.Package
import Alpm.Util

data Attr a b = Attr     (Alpm a) (a -> Alpm ())
              | ListAttr (Alpm a) (a -> Alpm ()) (b -> Alpm ()) (b -> Alpm ())

data AttrOp = forall a b. Attr a b :=  a
            | forall a b. Attr a b :~  (a -> a)
            | forall a b. Attr a b :++ b
            | forall a b. Attr a b :-- b

infixr 0 :=, :~

get :: Attr a b -> Alpm a
get (Attr     getter _    ) = getter
get (ListAttr getter _ _ _) = getter

set :: [AttrOp] -> Alpm ()
set = mapM_ app
  where
    app (Attr getter setter := x) = setter x
    app (Attr getter setter :~ f) = getter >>= setter . f

    app (ListAttr getter setter adder remover :=  x) = setter x
    app (ListAttr getter setter adder remover :~  f) = getter >>= setter . f
    app (ListAttr getter setter adder remover :++ x) = adder x
    app (ListAttr getter setter adder remover :-- x) = remover x

    app _ = fail "Operation not supported"

add :: Attr a b -> b -> Alpm ()
add (ListAttr _ _ adder _) x = adder x
add _                      _ = return ()

remove :: Attr a b -> b -> Alpm ()
remove (ListAttr _ _ _ remover) x = remover x
remove _                        _ = return ()

--------------------------------------------------------------------------------

newAlpmStringAttr getter setter = Attr alpmGetter alpmSetter
  where
    alpmGetter   = withAlpmPtr $ \ptr -> do
        ret <- getter ptr
        if isNull ret
            then return ""
            else peekCString ret
    alpmSetter v = withAlpmPtr $ \ptr -> newCString v >>= setter ptr

foreign import ccall "alpm_option_get_arch" c_alpm_option_get_arch :: Ptr AlpmHandle -> IO CString
foreign import ccall "alpm_option_set_arch" c_alpm_option_set_arch :: Ptr AlpmHandle -> CString -> IO ()
arch = newAlpmStringAttr c_alpm_option_get_arch
                         c_alpm_option_set_arch

foreign import ccall "alpm_option_get_logfile" c_alpm_option_get_logfile :: Ptr AlpmHandle -> IO CString
foreign import ccall "alpm_option_set_logfile" c_alpm_option_set_logfile :: Ptr AlpmHandle -> CString -> IO ()
logFile = newAlpmStringAttr c_alpm_option_get_logfile
                            c_alpm_option_set_logfile

foreign import ccall "alpm_option_get_gpgdir" c_alpm_option_get_gpgdir :: Ptr AlpmHandle -> IO CString
foreign import ccall "alpm_option_set_gpgdir" c_alpm_option_set_gpgdir :: Ptr AlpmHandle -> CString -> IO ()
gpgDirectory = newAlpmStringAttr c_alpm_option_get_gpgdir
                                 c_alpm_option_set_gpgdir

--------------------------------------------------------------------------------

newAlpmListAttr getter setter adder remove = ListAttr alpmGetter alpmSetter alpmAdder alpmRemover
  where
    alpmGetter    = withAlpmPtr $ \ptr -> boxAlpmList unsafePeekCString <$> getter ptr
    alpmSetter  v = withAlpmPtr $ \ptr -> undefined
    alpmAdder   v = withAlpmPtr $ \ptr -> newCString v >>= adder ptr
    alpmRemover v = withAlpmPtr $ \ptr -> newCString v >>= remove ptr

foo :: a -> Alpm ()
foo a = return ()

foreign import ccall "alpm_option_get_cachedirs"   c_alpm_option_get_cachedirs   :: Ptr AlpmHandle -> IO (Ptr (AlpmList CChar))
foreign import ccall "alpm_option_set_cachedirs"   c_alpm_option_set_cachedirs   :: Ptr AlpmHandle -> Ptr (AlpmList CChar) -> IO ()
foreign import ccall "alpm_option_add_cachedir"    c_alpm_option_add_cachedir    :: Ptr AlpmHandle -> CString -> IO ()
foreign import ccall "alpm_option_remove_cachedir" c_alpm_option_remove_cachedir :: Ptr AlpmHandle -> CString -> IO ()
cachePath = newAlpmListAttr c_alpm_option_get_cachedirs
                            c_alpm_option_set_cachedirs
                            c_alpm_option_add_cachedir
                            c_alpm_option_remove_cachedir

--------------------------------------------------------------------------------

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
