{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module System.Alpm.PkgCache where

import Control.Applicative
import Control.Monad.Reader
import Data.Time
import Foreign.C
import Foreign.Ptr

import System.Alpm.Core
import System.Alpm.Database
import System.Alpm.Internal.List
import System.Alpm.Internal.Types
import System.Alpm.StringLike
import System.Alpm.Utils

import qualified System.Alpm.Unsafe.Database as UD
import qualified System.Alpm.Unsafe.Package as U

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

pkgOrigin :: Package -> PkgCache Origin
pkgOrigin = (return $!) . U.pkgOrigin

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

pkgSize :: Package -> PkgCache (Maybe Int)
pkgSize = (return $!) . U.pkgSize

pkgInstallSize :: Package -> PkgCache Int
pkgInstallSize = (return $!) . U.pkgInstallSize

pkgReason :: Package -> PkgCache Reason
pkgReason = (return $!) . U.pkgReason

pkgLicenses :: (StringLike a, AlpmType a) => Package -> PkgCache [a]
pkgLicenses = (return $!) . U.pkgLicenses

pkgGroups :: (StringLike a, AlpmType a) => Package -> PkgCache [a]
pkgGroups = (return $!) . U.pkgGroups

pkgDepends :: (StringLike a, AlpmType a) => Package -> PkgCache [a]
pkgDepends = (return $!) . U.pkgDepends

pkgOptDepends :: (StringLike a, AlpmType a) => Package -> PkgCache [a]
pkgOptDepends = (return $!) . U.pkgOptDepends

pkgConflicts :: (StringLike a, AlpmType a) => Package -> PkgCache [a]
pkgConflicts = (return $!) . U.pkgConflicts

pkgProvides :: (StringLike a, AlpmType a) => Package -> PkgCache [a]
pkgProvides = (return $!) . U.pkgProvides

pkgDeltas :: (StringLike a, AlpmType a) => Package -> PkgCache [a]
pkgDeltas = (return $!) . U.pkgDeltas

pkgReplaces :: (StringLike a, AlpmType a) => Package -> PkgCache [a]
pkgReplaces = (return $!) . U.pkgReplaces

pkgFiles :: (StringLike a, AlpmType a) => Package -> PkgCache [a]
pkgFiles = (return $!) . U.pkgFiles

pkgBackup :: (StringLike a, AlpmType a) => Package -> PkgCache [a]
pkgBackup = (return $!) . U.pkgBackup

pkgDatabase :: Package -> PkgCache Database
pkgDatabase = (return $!) . U.pkgDatabase

pkgBase64Sig :: StringLike a => Package -> PkgCache a
pkgBase64Sig = (return $!) . U.pkgBase64Sig
