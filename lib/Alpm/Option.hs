{-# LANGUAGE ExistentialQuantification, ForeignFunctionInterface #-}

module Alpm.Option where

import Control.Applicative
import Control.DeepSeq
import Control.Monad.Reader
import Data.Word
import System.IO.Unsafe
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

data AttrOp = forall a b. Attr a b := a
            | forall a b. Attr a b :~ (a -> a)

infixr 0 :=, :~

get :: Attr a b -> Alpm a
get (Attr     get _    ) = get
get (ListAttr get _ _ _) = get

set :: [AttrOp] -> Alpm ()
set = mapM_ app
  where
    app (Attr     get set := x)     = set x
    app (Attr     get set :~ f)     = get >>= set . f
    app (ListAttr get set _ _ := x) = set x
    app (ListAttr get set _ _ :~ f) = get >>= set . f

add :: Attr a b -> b -> Alpm ()
add (ListAttr _ _ add _) x = add x
add _                    _ = return ()

remove :: Attr a b -> b -> Alpm ()
remove (ListAttr _ _ _ remove) x = remove x
remove _                       _ = return ()

--------------------------------------------------------------------------------

newAlpmStringAttr get set = Attr alpmGetter alpmSetter
  where
    alpmGetter   = withAlpmPtr $ \ptr -> do
        ret <- get ptr
        if isNull ret
            then return ""
            else peekCString ret
    alpmSetter v = withAlpmPtr $ \ptr -> newCString v >>= set ptr

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

newAlpmListAttr get set add remove = ListAttr alpmGetter alpmSetter alpmAdder alpmRemover
  where
    alpmGetter    = withAlpmPtr $ \ptr -> boxAlpmList unsafePeekCString <$> get ptr
    alpmSetter  v = withAlpmPtr $ \ptr -> withForeignPtr (mkAlpmList (unsafePerformIO . newCString) v) $ set ptr
    alpmAdder   v = withAlpmPtr $ \ptr -> newCString v >>= add ptr
    alpmRemover v = withAlpmPtr $ \ptr -> newCString v >>= remove ptr

foreign import ccall "alpm_option_get_cachedirs"   c_alpm_option_get_cachedirs   :: Ptr AlpmHandle -> IO (Ptr (AlpmList CChar))
foreign import ccall "alpm_option_set_cachedirs"   c_alpm_option_set_cachedirs   :: Ptr AlpmHandle -> Ptr (AlpmList CChar) -> IO ()
foreign import ccall "alpm_option_add_cachedir"    c_alpm_option_add_cachedir    :: Ptr AlpmHandle -> CString -> IO ()
foreign import ccall "alpm_option_remove_cachedir" c_alpm_option_remove_cachedir :: Ptr AlpmHandle -> CString -> IO ()
cachePath = newAlpmListAttr c_alpm_option_get_cachedirs
                            c_alpm_option_set_cachedirs
                            c_alpm_option_add_cachedir
                            c_alpm_option_remove_cachedir

foreign import ccall "alpm_option_get_noupgrades"   c_alpm_option_get_noupgrades   :: Ptr AlpmHandle -> IO (Ptr (AlpmList CChar))
foreign import ccall "alpm_option_set_noupgrades"   c_alpm_option_set_noupgrades   :: Ptr AlpmHandle -> Ptr (AlpmList CChar) -> IO ()
foreign import ccall "alpm_option_add_noupgrade"    c_alpm_option_add_noupgrade    :: Ptr AlpmHandle -> CString -> IO ()
foreign import ccall "alpm_option_remove_noupgrade" c_alpm_option_remove_noupgrade :: Ptr AlpmHandle -> CString -> IO ()
noUpgrades = newAlpmListAttr c_alpm_option_get_noupgrades
                             c_alpm_option_set_noupgrades
                             c_alpm_option_add_noupgrade
                             c_alpm_option_remove_noupgrade

foreign import ccall "alpm_option_get_noextracts"   c_alpm_option_get_noextracts   :: Ptr AlpmHandle -> IO (Ptr (AlpmList CChar))
foreign import ccall "alpm_option_set_noextracts"   c_alpm_option_set_noextracts   :: Ptr AlpmHandle -> Ptr (AlpmList CChar) -> IO ()
foreign import ccall "alpm_option_add_noextract"    c_alpm_option_add_noextract    :: Ptr AlpmHandle -> CString -> IO ()
foreign import ccall "alpm_option_remove_noextract" c_alpm_option_remove_noextract :: Ptr AlpmHandle -> CString -> IO ()
noExtracts = newAlpmListAttr c_alpm_option_get_noextracts
                             c_alpm_option_set_noextracts
                             c_alpm_option_add_noextract
                             c_alpm_option_remove_noextract

foreign import ccall "alpm_option_get_ignorepkgs"   c_alpm_option_get_ignorepkgs   :: Ptr AlpmHandle -> IO (Ptr (AlpmList CChar))
foreign import ccall "alpm_option_set_ignorepkgs"   c_alpm_option_set_ignorepkgs   :: Ptr AlpmHandle -> Ptr (AlpmList CChar) -> IO ()
foreign import ccall "alpm_option_add_ignorepkg"    c_alpm_option_add_ignorepkg    :: Ptr AlpmHandle -> CString -> IO ()
foreign import ccall "alpm_option_remove_ignorepkg" c_alpm_option_remove_ignorepkg :: Ptr AlpmHandle -> CString -> IO ()
ignorePkgs = newAlpmListAttr c_alpm_option_get_ignorepkgs
                             c_alpm_option_set_ignorepkgs
                             c_alpm_option_add_ignorepkg
                             c_alpm_option_remove_ignorepkg

foreign import ccall "alpm_option_get_ignoregroups"   c_alpm_option_get_ignoregroups   :: Ptr AlpmHandle -> IO (Ptr (AlpmList CChar))
foreign import ccall "alpm_option_set_ignoregroups"   c_alpm_option_set_ignoregroups   :: Ptr AlpmHandle -> Ptr (AlpmList CChar) -> IO ()
foreign import ccall "alpm_option_add_ignoregroup"    c_alpm_option_add_ignoregroup    :: Ptr AlpmHandle -> CString -> IO ()
foreign import ccall "alpm_option_remove_ignoregroup" c_alpm_option_remove_ignoregroup :: Ptr AlpmHandle -> CString -> IO ()
ignoreGroups = newAlpmListAttr c_alpm_option_get_ignoregroups
                               c_alpm_option_set_ignoregroups
                               c_alpm_option_add_ignoregroup
                               c_alpm_option_remove_ignoregroup

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
