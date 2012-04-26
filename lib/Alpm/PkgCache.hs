{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Alpm.PkgCache where

import Control.Applicative
import Control.Monad.Reader
import Data.Time
import Foreign.C
import Foreign.Ptr

import Alpm.Core
import Alpm.Database
import Alpm.Internal.List
import Alpm.Internal.Types
import Alpm.StringLike
import Alpm.Utils

import qualified Alpm.Unsafe.Database as UD
import qualified Alpm.Unsafe.Package as U

newtype PkgCache a = PkgCache (ReaderT [Package] Alpm a)
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader [Package])

withPkgCache :: Database -> PkgCache a -> Alpm a
withPkgCache db (PkgCache f) = UD.pkgCache db >>= runReaderT f

withPkgCaches :: [Database] -> PkgCache a -> Alpm a
withPkgCaches dbs (PkgCache f) = concat <$> mapM UD.pkgCache dbs >>= runReaderT f

---------------------------------------------------------------------

pkgFilename :: StringLike a => Package -> PkgCache (Maybe a)
pkgFilename = (return $!) . U.pkgFilename

pkgName :: StringLike a => Package -> PkgCache a
pkgName = (return $!) . U.pkgName

pkgVersion :: StringLike a => Package -> PkgCache a
pkgVersion = (return $!) . U.pkgVersion

pkgOrigin = undefined

pkgDescription :: StringLike a => Package -> PkgCache a
pkgDescription = (return $!) . U.pkgDescription

pkgURL :: StringLike a => Package -> PkgCache a
pkgURL = (return $!) . U.pkgURL

pkgBuildDate :: Package -> PkgCache UTCTime
pkgBuildDate = (return $!) . U.pkgBuildDate

pkgInstallDate :: Package -> PkgCache UTCTime
pkgInstallDate = (return $!) . U.pkgInstallDate

pkgPackager :: StringLike a => Package -> PkgCache a
pkgPackager = (return $!) . U.pkgPackager

pkgMD5Sum :: StringLike a => Package -> PkgCache (Maybe a)
pkgMD5Sum = (return $!) . U.pkgMD5Sum

pkgSHA256Sum :: StringLike a => Package -> PkgCache (Maybe a)
pkgSHA256Sum = (return $!) . U.pkgSHA256Sum

pkgArch :: StringLike a => Package -> PkgCache a
pkgArch = (return $!) . U.pkgArch

pkgSize = undefined

pkgInstallSize = undefined

pkgReason = undefined

pkgLicenses = undefined

pkgGroups :: (StringLike a, AlpmType a) => Package -> PkgCache [a]
pkgGroups = (return $!) . U.pkgGroups

pkgDatabase :: Package -> PkgCache Database
pkgDatabase = (return $!) . U.pkgDatabase
