{-# LANGUAGE ExistentialQuantification, ForeignFunctionInterface #-}

{# context lib="alpm" prefix="alpm_option" #}

module Alpm.Options where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import System.Posix.Unistd
import Foreign.C
import Foreign.Ptr
import Foreign.Marshal.Utils (toBool, fromBool)

import Alpm.Core
import Alpm.Internal.Callbacks
import Alpm.Callbacks
import Alpm.StringLike
import Alpm.Utils

#include <alpm.h>

data Attr a = Attr (Alpm a) (a -> Alpm ())

data AttrOp = forall a. Attr a :=  a
            | forall a. Attr a :~  (a -> a)
            | forall a. Attr a :=> (Alpm a)
            | forall a. Attr a :~> (a -> Alpm a)

infixr 0 :=, :~, :=>, :~>

get :: Attr a -> Alpm a
get (Attr get _) = get

set :: [AttrOp] -> Alpm ()
set = mapM_ app
  where
    app (Attr get set :=  x) = set x
    app (Attr get set :~  f) = get >>= set . f
    app (Attr get set :=> x) =   x >>= set
    app (Attr get set :~> f) = get >>= f >>= set

---------------------------------------------------------------------

mkStringAttr get set = Attr getter setter
  where getter   = withHandle $ readString . get
        setter v = void . withHandle $ (toC v >>=) . set

arch :: StringLike a => Attr a
arch = mkStringAttr {# call get_arch #} {# call set_arch #}

logFile :: StringLike a => Attr a
logFile = mkStringAttr {# call get_logfile #} {# call set_logfile #}

gpgDirectory :: StringLike a => Attr a
gpgDirectory = mkStringAttr {# call get_gpgdir #} {# call set_gpgdir #}

---------------------------------------------------------------------

mkBoolAttr get set = Attr getter setter
  where getter   = withHandle $ (toBool <$>) . get
        setter v = void $ withHandle (`set` fromBool v)

useSyslog :: Attr Bool
useSyslog = mkBoolAttr {# call get_usesyslog #} {# call set_usesyslog #}

checkSpace :: Attr Bool
checkSpace = mkBoolAttr {# call get_checkspace #} {# call set_checkspace #}

---------------------------------------------------------------------

systemArch = arch :=> liftIO $ machine <$> getSystemID

---------------------------------------------------------------------

type LogCallback = Int -> String -> IO ()
type DownloadCallback = String -> Int -> Int -> IO ()

foreign import ccall "alpm_option_set_logcb"
    set_logcb :: Ptr () -> FunPtr LogFunc -> IO CInt

onLog :: Maybe LogCallback -> Alpm ()
onLog = setCallback LogCB set_logcb logFunc

onDownload :: Maybe DownloadCallback -> Alpm ()
onDownload = setCallback DownloadCB {# call set_dlcb #} downloadFunc

logFunc :: LogCallback -> IO (FunPtr LogFunc)
logFunc f = mkLogFunc $ \lvl str ->
    peekCString str >>= f (fromIntegral lvl) . reverse . drop 1 . reverse

downloadFunc :: DownloadCallback -> IO (FunPtr DownloadFunc)
downloadFunc f = mkDownloadFunc $ \name xfer total -> do
    let xfer'  = fromIntegral xfer
        total' = fromIntegral total
    name' <- peekCString name
    f name' xfer' total'
