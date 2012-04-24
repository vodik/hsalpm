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
  where getter   = withHandle $ toString . get
        setter v = void . withHandle $ (newCString v >>=) . set

arch :: Attr String
arch = mkStringAttr {# call get_arch #} {# call set_arch #}

logFile :: Attr FilePath
logFile = mkStringAttr {# call get_logfile #} {# call set_logfile #}

gpgDirectory :: Attr FilePath
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
