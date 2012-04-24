{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Alpm.PkgCache where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Data.Time
import Foreign.C
import Foreign.Ptr
import Control.Monad.Reader

import Alpm.Core
import Alpm.Database
import Alpm.Internal.List
import Alpm.Internal.Types
import Alpm.Unsafe.Package (Package)
import Alpm.Utils
import qualified Alpm.Unsafe.Database as UD
import qualified Alpm.Unsafe.Package as U

newtype PkgCache a = PkgCache (ReaderT [Package] Alpm a)
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader [Package])

withPkgCache :: Database -> PkgCache a -> Alpm a
withPkgCache db (PkgCache f) = UD.pkgCache db >>= runReaderT f

---------------------------------------------------------------------

pkgFilename :: Package -> PkgCache (Maybe String)
pkgFilename = U.pkgFilename

pkgName :: Package -> PkgCache String
pkgName = U.pkgName

pkgVersion :: Package -> PkgCache String
pkgVersion = U.pkgVersion

pkgOrigin = undefined

pkgDescription :: Package -> PkgCache String
pkgDescription = U.pkgDescription

pkgURL :: Package -> PkgCache String
pkgURL = U.pkgURL

pkgBuildDate :: Package -> PkgCache UTCTime
pkgBuildDate = U.pkgBuildDate

pkgInstallDate :: Package -> PkgCache UTCTime
pkgInstallDate = U.pkgInstallDate

pkgPackager :: Package -> PkgCache String
pkgPackager = U.pkgPackager

pkgMD5Sum :: Package -> PkgCache (Maybe String)
pkgMD5Sum = U.pkgMD5Sum

pkgSHA256Sum :: Package -> PkgCache (Maybe String)
pkgSHA256Sum = U.pkgSHA256Sum

pkgArch :: Package -> PkgCache String
pkgArch = U.pkgArch

pkgSize = undefined

pkgInstallSize = undefined

pkgReason = undefined

pkgLicenses = undefined

pkgGroups = undefined
