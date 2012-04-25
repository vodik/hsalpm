{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Alpm.Core
    ( Alpm
    , AlpmOptions(..)
    , defaultOptions
    , withHandle

    , runAlpm
    ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Error
import Control.Monad.Reader
import Foreign.ForeignPtr

import Alpm.Internal.Alpm

data AlpmEnv = AlpmEnv
    { handle :: Handle
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

defaultOptions = AlpmOptions
    { root   = "/"
    , dbPath = "/var/lib/pacman"
    }

runAlpm :: AlpmOptions -> Alpm a -> IO (Either AlpmException a)
runAlpm opt (Alpm f) =
    alpmInitialize (root opt) (dbPath opt) >>= either failed run
  where
    failed  = return . Left . AlpmException "failed to initialize alpm library"
    run env = withForeignPtr env . const $ runReaderT (runErrorT f) (AlpmEnv env)

withHandle f = ask >>= liftIO . flip withForeignPtr f . handle
