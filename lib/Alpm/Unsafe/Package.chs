{-# LANGUAGE ForeignFunctionInterface #-}

{# context lib="alpm" prefix="alpm" #}

module Alpm.Unsafe.Package where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Data.Time
import Foreign.C
import Foreign.Ptr
import Control.Monad.Reader

import Alpm.Internal.List
import Alpm.Internal.Types
import Alpm.Utils

#include <alpm.h>

{# pointer *alpm_pkg_t as Package newtype #}

instance AlpmType Package where
    unpack (Package ptr) = return ptr
    pack = return . Package

pkgFilename :: MonadIO m => Package -> m (Maybe String)
pkgFilename = maybeString . {# call pkg_get_filename #}

pkgName :: MonadIO m => Package -> m String
pkgName = toString . {# call pkg_get_name #}

pkgVersion :: MonadIO m => Package -> m String
pkgVersion = toString . {# call pkg_get_version #}

pkgOrigin = undefined

pkgDescription :: MonadIO m => Package -> m String
pkgDescription = toString . {# call pkg_get_desc #}

pkgURL :: MonadIO m => Package -> m String
pkgURL = toString . {# call pkg_get_url #}

pkgBuildDate :: MonadIO m => Package -> m UTCTime
pkgBuildDate = toDate . {# call pkg_get_builddate #}

pkgInstallDate :: MonadIO m => Package -> m UTCTime
pkgInstallDate = toDate . {# call pkg_get_installdate #}

pkgPackager :: MonadIO m => Package -> m String
pkgPackager = toString . {# call pkg_get_packager #}

pkgMD5Sum :: MonadIO m => Package -> m (Maybe String)
pkgMD5Sum = maybeString . {# call pkg_get_md5sum #}

pkgSHA256Sum :: MonadIO m => Package -> m (Maybe String)
pkgSHA256Sum = maybeString . {# call pkg_get_sha256sum #}

pkgArch :: MonadIO m => Package -> m String
pkgArch = toString . {# call pkg_get_arch #}

pkgSize = undefined

pkgInstallSize = undefined

pkgReason = undefined

pkgLicenses = undefined

pkgGroups = undefined
