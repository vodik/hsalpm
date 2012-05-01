{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module System.Alpm.Core
    ( Alpm
    , AlpmError(..)
    , AlpmHandle(..)
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
import System.Alpm.Internal.Types
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

withHandle :: (Ptr () -> IO a) -> Alpm a
withHandle = (ask >>=) . (liftIO .) . flip withForeignPtr

lastError :: Alpm ErrorCode
lastError = withHandle errno

throwAlpmException :: String -> Alpm a
throwAlpmException = (throwError =<<) . (<$> lastError) . flip Library

withAlpm :: FilePath -> FilePath -> Alpm a -> IO (Either AlpmError a)
withAlpm root dbPath alpm =
    alpmInitialize root dbPath >>= either failed run
  where
    failed  = return . Left . flip Library "failed to initialize alpm"
    run env = withForeignPtr env . const . (`runReaderT` env) . runErrorT $ runAlpm alpm
