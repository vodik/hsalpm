{-# LANGUAGE ForeignFunctionInterface #-}

{# context lib="alpm" prefix="alpm" #}

module Alpm.Unsafe.Package where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Reader
import Data.Time
import Foreign.C
import Foreign.Ptr
import System.IO.Unsafe

import Alpm.Internal.List
import Alpm.Internal.Types
import Alpm.Utils

#include <alpm.h>

{# pointer *alpm_pkg_t as Package newtype #}

instance AlpmType Package where
    unpack (Package ptr) = return ptr
    pack = return . Package

pkgFilename :: Package -> (Maybe String)
pkgFilename = unsafePerformIO . maybeString . {# call pkg_get_filename #}

pkgName :: Package -> String
pkgName = unsafePerformIO . toString . {# call pkg_get_name #}

pkgVersion :: Package -> String
pkgVersion = unsafePerformIO . toString . {# call pkg_get_version #}

pkgOrigin = undefined

pkgDescription :: Package -> String
pkgDescription = unsafePerformIO . toString . {# call pkg_get_desc #}

pkgURL :: Package -> String
pkgURL = unsafePerformIO . toString . {# call pkg_get_url #}

pkgBuildDate :: Package -> UTCTime
pkgBuildDate = unsafePerformIO . toDate . {# call pkg_get_builddate #}

pkgInstallDate :: Package -> UTCTime
pkgInstallDate = unsafePerformIO . toDate . {# call pkg_get_installdate #}

pkgPackager :: Package -> String
pkgPackager = unsafePerformIO . toString . {# call pkg_get_packager #}

pkgMD5Sum :: Package -> (Maybe String)
pkgMD5Sum = unsafePerformIO . maybeString . {# call pkg_get_md5sum #}

pkgSHA256Sum :: Package -> (Maybe String)
pkgSHA256Sum = unsafePerformIO . maybeString . {# call pkg_get_sha256sum #}

pkgArch :: Package -> String
pkgArch = unsafePerformIO . toString . {# call pkg_get_arch #}

pkgSize = undefined

pkgInstallSize = undefined

pkgReason = undefined

pkgLicenses = undefined

pkgGroups = undefined
