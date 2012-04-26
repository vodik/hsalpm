{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Alpm.Core
    ( Alpm
    , AlpmException(..)
    -- , AlpmEnv(..)
    , AlpmHandle(..)
    , AlpmOptions(..)
    , defaultOptions
    , withHandle
    , throwAlpmException

    , runAlpm
    ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.State
import Data.IORef
import Foreign.ForeignPtr
import Foreign.Ptr

import Alpm.Internal.Alpm

type AlpmEnv = IORef Env
data Env = Env
    { handle :: Int
    }

data AlpmOptions = AlpmOptions
    { root   :: FilePath
    , dbPath :: FilePath
    }

data AlpmException = AlpmException String String
                   | UnknownException
                   deriving (Eq, Read, Show)

newtype Alpm a = Alpm (ErrorT AlpmException (ReaderT AlpmHandle (StateT AlpmEnv IO)) a)
    deriving (Functor, Applicative, Monad, MonadIO, MonadError AlpmException, MonadState AlpmEnv, MonadReader AlpmHandle)

instance Error AlpmException where
    noMsg    = UnknownException
    strMsg _ = UnknownException

defaultOptions = AlpmOptions
    { root   = "/"
    , dbPath = "/var/lib/pacman"
    }

withHandle :: (Ptr () -> IO a) -> Alpm a
withHandle f = ask >>= liftIO . flip withForeignPtr f

lastStrerror :: Alpm String
lastStrerror = withHandle $ (strerror =<<) . errno

throwAlpmException :: String -> Alpm a
throwAlpmException = (throwError =<<) . (<$> lastStrerror) . AlpmException

runAlpm :: AlpmOptions -> Alpm a -> IO (Either AlpmException a)
runAlpm opt (Alpm f) = do
    r <- newIORef $ Env 0
    alpmInitialize (root opt) (dbPath opt) >>= either failed (run r)
  where
    failed    = return . Left . AlpmException "failed to initialize alpm library"
    run r env = withForeignPtr env . const $ evalStateT (runReaderT (runErrorT f) env) r
