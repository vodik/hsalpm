{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Alpm.Core
    ( Alpm(..)
    , AlpmEnv(..)
    , AlpmException(..)
    , AlpmOptions(..)
    , defaultOptions
    , withHandle
    , throwAlpmException

    -- , runAlpm
    ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Error
import Control.Monad.Reader
import Data.IORef
import Foreign.ForeignPtr
import Foreign.Ptr
import qualified Data.Map as M

import Alpm.Internal.Alpm

data AlpmEnv = AlpmEnv
    { handle :: AlpmHandle
    }

data AlpmOptions = AlpmOptions
    { root   :: FilePath
    , dbPath :: FilePath
    }

data AlpmException = AlpmException String String
                   | UnknownException
                   deriving (Eq, Read, Show)

newtype Alpm a = Alpm (ErrorT AlpmException (ReaderT AlpmEnv IO) a)
    deriving (Functor, Applicative, Monad, MonadIO, MonadError AlpmException, MonadReader AlpmEnv)

instance Error AlpmException where
    noMsg    = UnknownException
    strMsg _ = UnknownException

withHandle :: (Ptr () -> IO a) -> Alpm a
withHandle f = ask >>= liftIO . flip withForeignPtr f . handle

lastStrerror :: Alpm String
lastStrerror = withHandle $ (strerror =<<) . errno

throwAlpmException :: String -> Alpm a
throwAlpmException = (throwError =<<) . (<$> lastStrerror) . AlpmException

defaultOptions :: AlpmOptions
defaultOptions = AlpmOptions
    { root   = "/"
    , dbPath = "/var/cache/pacman"
    }
