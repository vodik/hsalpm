{-# LANGUAGE ExistentialQuantification, ForeignFunctionInterface #-}

{# context lib="alpm" prefix="alpm" #}

module Alpm.Options where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import System.Posix.Unistd
import Foreign.C
import Foreign.Ptr

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
arch = mkStringAttr {# call option_get_arch #}
                    {# call option_set_arch #}

logFile :: Attr FilePath
logFile = mkStringAttr {# call option_get_logfile #}
                       {# call option_set_logfile #}

gpgDirectory :: Attr FilePath
gpgDirectory = mkStringAttr {# call option_get_gpgdir #}
                            {# call option_set_gpgdir #}

---------------------------------------------------------------------

systemArch = arch :=> liftIO $ machine <$> getSystemID
