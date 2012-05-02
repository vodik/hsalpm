{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module System.Alpm.PkgCache where

import Control.Applicative
import Control.Monad.Error
import Control.Monad.Reader
import Data.Time
import Foreign.C
import Foreign.Ptr

import System.Alpm.Core
import System.Alpm.Database
import System.Alpm.Cache
import System.Alpm.Internal.List
import System.Alpm.Internal.Types
import System.Alpm.StringLike
import System.Alpm.Utils

import qualified System.Alpm.Unsafe.Database as UD

newtype PkgCache a = PkgCache (ReaderT [Package] Alpm a)
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader [Package], MonadError AlpmError)

instance MonadCache PkgCache where
    cache = ask

withPkgCache :: Database -> PkgCache a -> Alpm a
withPkgCache db (PkgCache f) = UD.pkgCache db >>= runReaderT f
