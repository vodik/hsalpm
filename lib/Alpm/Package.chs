{-# LANGUAGE ForeignFunctionInterface #-}

{# context lib="alpm" prefix="alpm" #}

module Alpm.Package where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Data.Time
import Foreign.C
import Foreign.Ptr

import Alpm.Core
import Alpm.Internal.List
import Alpm.Internal.Types
import Alpm.Utils

#include <alpm.h>

{# pointer *alpm_pkg_t as Package newtype #}

instance AlpmType Package where
    unpack (Package ptr) = return ptr
    pack = return . Package

pkgFilename :: Package -> Alpm (Maybe String)
pkgFilename = maybeString . {# call pkg_get_filename #}

pkgName :: Package -> Alpm String
pkgName = toString . {# call pkg_get_name #}

pkgVersion :: Package -> Alpm String
pkgVersion = toString . {# call pkg_get_version #}

pkgOrigin = undefined

pkgDescription :: Package -> Alpm String
pkgDescription = toString . {# call pkg_get_desc #}

pkgURL :: Package -> Alpm String
pkgURL = toString . {# call pkg_get_url #}

pkgBuildDate :: Package -> Alpm UTCTime
pkgBuildDate = toDate . {# call pkg_get_builddate #}

pkgInstallDate :: Package -> Alpm UTCTime
pkgInstallDate = toDate . {# call pkg_get_installdate #}

pkgPackager :: Package -> Alpm String
pkgPackager = toString . {# call pkg_get_packager #}

pkgMD5Sum :: Package -> Alpm (Maybe String)
pkgMD5Sum = maybeString . {# call pkg_get_md5sum #}

pkgSHA256Sum :: Package -> Alpm (Maybe String)
pkgSHA256Sum = maybeString . {# call pkg_get_sha256sum #}

pkgArch :: Package -> Alpm String
pkgArch = toString . {# call pkg_get_arch #}

pkgSize = undefined

pkgInstallSize = undefined

pkgReason = undefined

pkgLicenses = undefined

pkgGroups = undefined
