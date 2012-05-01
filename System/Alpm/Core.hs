{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module System.Alpm.Core
    ( Alpm
    , AlpmError(..)
    , AlpmHandle(..)
    , AlpmOptions(..)
    , defaultOptions
    , withHandle
    , throwAlpmException
    , withAlpm
    ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.State
import Data.IORef
import Foreign.ForeignPtr
import Foreign.Ptr

import System.Alpm.Internal.Alpm
import System.Alpm.Core.Class
import System.Alpm.Core.Error

-- type AlpmEnv = IORef Env
-- data Env = Env { handle :: Int }

data AlpmOptions = AlpmOptions
    { root   :: FilePath
    , dbPath :: FilePath
    }

newtype Alpm a = Alpm
    { runAlpm :: ErrorT AlpmError (ReaderT AlpmHandle IO) a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadError AlpmError, MonadReader AlpmHandle)

defaultOptions = AlpmOptions
    { root   = "/"
    , dbPath = "/var/lib/pacman"
    }

withHandle :: (Ptr () -> IO a) -> Alpm a
withHandle f = ask >>= liftIO . flip withForeignPtr f

lastStrerror :: Alpm String
lastStrerror = withHandle $ (strerror =<<) . errno

throwAlpmException :: String -> Alpm a
throwAlpmException = (throwError =<<) . (<$> lastStrerror) . Generic

withAlpm :: AlpmOptions -> Alpm a -> IO (Either AlpmError a)
withAlpm opt alpm =
    alpmInitialize (root opt) (dbPath opt) >>= either failed run
  where
    failed  = return . Left . Generic "failed to initialize alpm library"
    run env = withForeignPtr env . const $ (`runReaderT` env) . runErrorT $ runAlpm alpm
