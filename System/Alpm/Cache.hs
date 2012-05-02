{-# LANGUAGE FlexibleContexts #-}

module System.Alpm.Cache where

import Control.Monad.Error
import Data.Time

import System.Alpm.Core.Error
import System.Alpm.Internal.Types
import System.Alpm.StringLike
import qualified System.Alpm.Unsafe.Package as U

class (Monad m, MonadError AlpmError m) => MonadPkgCache m where
    cache :: m [Package]

    add    :: Package -> m ()
    remove :: Package -> m ()

----------------------------------------------------------------------

pkgFilename :: (MonadPkgCache m, StringLike a) => Package -> m (Maybe a)
pkgFilename = return . U.pkgFilename

pkgName :: (MonadPkgCache m, StringLike a) => Package -> m a
pkgName = return . U.pkgName

pkgVersion :: (MonadPkgCache m, StringLike a) => Package -> m a
pkgVersion = return . U.pkgVersion

pkgOrigin :: MonadPkgCache m => Package -> m Origin
pkgOrigin = return . U.pkgOrigin

pkgDescription :: (MonadPkgCache m, StringLike a) => Package -> m a
pkgDescription = return . U.pkgDescription

pkgURL :: (MonadPkgCache m, StringLike a) => Package -> m a
pkgURL = return . U.pkgURL

pkgBuildDate :: MonadPkgCache m => Package -> m UTCTime
pkgBuildDate = return . U.pkgBuildDate

pkgInstallDate :: MonadPkgCache m => Package -> m UTCTime
pkgInstallDate = return . U.pkgInstallDate

pkgPackager :: (MonadPkgCache m, StringLike a) => Package -> m a
pkgPackager = return . U.pkgPackager

pkgMD5Sum :: (MonadPkgCache m, StringLike a) => Package -> m (Maybe a)
pkgMD5Sum = return . U.pkgMD5Sum

pkgSHA256Sum :: (MonadPkgCache m, StringLike a) => Package -> m (Maybe a)
pkgSHA256Sum = return . U.pkgSHA256Sum

pkgArch :: (MonadPkgCache m, StringLike a) => Package -> m a
pkgArch = return . U.pkgArch

pkgSize :: MonadPkgCache m => Package -> m (Maybe Int)
pkgSize = return . U.pkgSize

pkgInstallSize :: MonadPkgCache m => Package -> m Int
pkgInstallSize = return . U.pkgInstallSize

pkgReason :: MonadPkgCache m => Package -> m Reason
pkgReason = return . U.pkgReason

pkgLicenses :: (MonadPkgCache m, StringLike a, AlpmType a) => Package -> m [a]
pkgLicenses = return . U.pkgLicenses

pkgGroups :: (MonadPkgCache m, StringLike a, AlpmType a) => Package -> m [a]
pkgGroups = return . U.pkgGroups

pkgDepends :: (MonadPkgCache m, StringLike a, AlpmType a) => Package -> m [a]
pkgDepends = return . U.pkgDepends

pkgOptDepends :: (MonadPkgCache m, StringLike a, AlpmType a) => Package -> m [a]
pkgOptDepends = return . U.pkgOptDepends

pkgConflicts :: (MonadPkgCache m, StringLike a, AlpmType a) => Package -> m [a]
pkgConflicts = return . U.pkgConflicts

pkgProvides :: (MonadPkgCache m, StringLike a, AlpmType a) => Package -> m [a]
pkgProvides = return . U.pkgProvides

pkgDeltas :: (MonadPkgCache m, StringLike a, AlpmType a) => Package -> m [a]
pkgDeltas = return . U.pkgDeltas

pkgReplaces :: (MonadPkgCache m, StringLike a, AlpmType a) => Package -> m [a]
pkgReplaces = return . U.pkgReplaces

pkgFiles :: (MonadPkgCache m, StringLike a, AlpmType a) => Package -> m [a]
pkgFiles = return . U.pkgFiles

pkgBackup :: (MonadPkgCache m, StringLike a, AlpmType a) => Package -> m [a]
pkgBackup = return . U.pkgBackup

pkgDatabase :: MonadPkgCache m => Package -> m Database
pkgDatabase = return . U.pkgDatabase

pkgBase64Sig :: (MonadPkgCache m, StringLike a) => Package -> m a
pkgBase64Sig = return . U.pkgBase64Sig
