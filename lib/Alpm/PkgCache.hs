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

pkgFilename :: Package -> PkgCache (Maybe String)
pkgFilename = (return $!) . U.pkgFilename

pkgName :: Package -> PkgCache String
pkgName = (return $!) . U.pkgName

pkgVersion :: Package -> PkgCache String
pkgVersion = (return $!) . U.pkgVersion

pkgOrigin = undefined

pkgDescription :: Package -> PkgCache String
pkgDescription = (return $!) . U.pkgDescription

pkgURL :: Package -> PkgCache String
pkgURL = (return $!) . U.pkgURL

pkgBuildDate :: Package -> PkgCache UTCTime
pkgBuildDate = (return $!) . U.pkgBuildDate

pkgInstallDate :: Package -> PkgCache UTCTime
pkgInstallDate = (return $!) . U.pkgInstallDate

pkgPackager :: Package -> PkgCache String
pkgPackager = (return $!) . U.pkgPackager

pkgMD5Sum :: Package -> PkgCache (Maybe String)
pkgMD5Sum = (return $!) . U.pkgMD5Sum

pkgSHA256Sum :: Package -> PkgCache (Maybe String)
pkgSHA256Sum = (return $!) . U.pkgSHA256Sum

pkgArch :: Package -> PkgCache String
pkgArch = (return $!) . U.pkgArch

pkgSize = undefined

pkgInstallSize = undefined

pkgReason = undefined

pkgLicenses = undefined

pkgGroups :: Package -> PkgCache [String]
pkgGroups = (return $!) . U.pkgGroups

pkgDatabase :: Package -> PkgCache Database
pkgDatabase = (return $!) . U.pkgDatabase
