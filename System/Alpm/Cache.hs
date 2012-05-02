{-# LANGUAGE FlexibleContexts #-}

module System.Alpm.Cache where

import Control.Monad.Error
import Data.Time

import System.Alpm.Core.Error
import System.Alpm.Internal.Types
import System.Alpm.StringLike
import qualified System.Alpm.Unsafe.Package as U

data Action = Add | Remove

class (Monad m, MonadError AlpmError m) => MonadCache m where
    cache :: m [Package]

class (Monad m, MonadError AlpmError m) => MonadStage m where
    stage :: Action -> Package -> m ()

stageEach :: MonadStage m => Action -> [Package] -> m ()
stageEach = mapM_ . stage

----------------------------------------------------------------------

pkgFilename :: (MonadCache m, StringLike a) => Package -> m (Maybe a)
pkgFilename = return . U.pkgFilename

pkgName :: (MonadCache m, StringLike a) => Package -> m a
pkgName = return . U.pkgName

pkgVersion :: (MonadCache m, StringLike a) => Package -> m a
pkgVersion = return . U.pkgVersion

pkgOrigin :: MonadCache m => Package -> m Origin
pkgOrigin = return . U.pkgOrigin

pkgDescription :: (MonadCache m, StringLike a) => Package -> m a
pkgDescription = return . U.pkgDescription

pkgURL :: (MonadCache m, StringLike a) => Package -> m a
pkgURL = return . U.pkgURL

pkgBuildDate :: MonadCache m => Package -> m UTCTime
pkgBuildDate = return . U.pkgBuildDate

pkgInstallDate :: MonadCache m => Package -> m UTCTime
pkgInstallDate = return . U.pkgInstallDate

pkgPackager :: (MonadCache m, StringLike a) => Package -> m a
pkgPackager = return . U.pkgPackager

pkgMD5Sum :: (MonadCache m, StringLike a) => Package -> m (Maybe a)
pkgMD5Sum = return . U.pkgMD5Sum

pkgSHA256Sum :: (MonadCache m, StringLike a) => Package -> m (Maybe a)
pkgSHA256Sum = return . U.pkgSHA256Sum

pkgArch :: (MonadCache m, StringLike a) => Package -> m a
pkgArch = return . U.pkgArch

pkgSize :: MonadCache m => Package -> m (Maybe Int)
pkgSize = return . U.pkgSize

pkgInstallSize :: MonadCache m => Package -> m Int
pkgInstallSize = return . U.pkgInstallSize

pkgReason :: MonadCache m => Package -> m Reason
pkgReason = return . U.pkgReason

pkgLicenses :: (MonadCache m, StringLike a, AlpmType a) => Package -> m [a]
pkgLicenses = return . U.pkgLicenses

pkgGroups :: (MonadCache m, StringLike a, AlpmType a) => Package -> m [a]
pkgGroups = return . U.pkgGroups

pkgDepends :: (MonadCache m, StringLike a, AlpmType a) => Package -> m [a]
pkgDepends = return . U.pkgDepends

pkgOptDepends :: (MonadCache m, StringLike a, AlpmType a) => Package -> m [a]
pkgOptDepends = return . U.pkgOptDepends

pkgConflicts :: (MonadCache m, StringLike a, AlpmType a) => Package -> m [a]
pkgConflicts = return . U.pkgConflicts

pkgProvides :: (MonadCache m, StringLike a, AlpmType a) => Package -> m [a]
pkgProvides = return . U.pkgProvides

pkgDeltas :: (MonadCache m, StringLike a, AlpmType a) => Package -> m [a]
pkgDeltas = return . U.pkgDeltas

pkgReplaces :: (MonadCache m, StringLike a, AlpmType a) => Package -> m [a]
pkgReplaces = return . U.pkgReplaces

pkgFiles :: (MonadCache m, StringLike a, AlpmType a) => Package -> m [a]
pkgFiles = return . U.pkgFiles

pkgBackup :: (MonadCache m, StringLike a, AlpmType a) => Package -> m [a]
pkgBackup = return . U.pkgBackup

pkgDatabase :: MonadCache m => Package -> m Database
pkgDatabase = return . U.pkgDatabase

pkgBase64Sig :: (MonadCache m, StringLike a) => Package -> m a
pkgBase64Sig = return . U.pkgBase64Sig
