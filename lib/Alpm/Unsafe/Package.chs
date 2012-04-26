{-# LANGUAGE ForeignFunctionInterface #-}

{# context lib="alpm" prefix="alpm_pkg" #}

module Alpm.Unsafe.Package where

import Control.Applicative
import Control.Monad.Trans
import Data.Time
import Foreign.C
import Foreign.Ptr
import System.IO.Unsafe

import Alpm.Database
import Alpm.Internal.List
import Alpm.Internal.Types
import Alpm.StringLike
import Alpm.Utils

{# import Alpm.Internal.Types #}

#include <alpm.h>

pkgFilename :: StringLike a => Package -> (Maybe a)
pkgFilename = unsafePerformIO . maybeString . {# call get_filename #}

pkgName :: StringLike a => Package -> a
pkgName = unsafePerformIO . readString . {# call get_name #}

pkgVersion :: StringLike a => Package -> a
pkgVersion = unsafePerformIO . readString . {# call get_version #}

pkgOrigin = undefined

pkgDescription :: StringLike a => Package -> a
pkgDescription = unsafePerformIO . readString . {# call get_desc #}

pkgURL :: StringLike a => Package -> a
pkgURL = unsafePerformIO . readString . {# call get_url #}

pkgBuildDate :: Package -> UTCTime
pkgBuildDate = unsafePerformIO . toDate . {# call get_builddate #}

pkgInstallDate :: Package -> UTCTime
pkgInstallDate = unsafePerformIO . toDate . {# call get_installdate #}

pkgPackager :: StringLike a => Package -> a
pkgPackager = unsafePerformIO . readString . {# call get_packager #}

pkgMD5Sum :: StringLike a => Package -> (Maybe a)
pkgMD5Sum = unsafePerformIO . maybeString . {# call get_md5sum #}

pkgSHA256Sum :: StringLike a => Package -> (Maybe a)
pkgSHA256Sum = unsafePerformIO . maybeString . {# call get_sha256sum #}

pkgArch :: StringLike a => Package -> a
pkgArch = unsafePerformIO . readString . {# call get_arch #}

pkgSize = undefined

pkgInstallSize = undefined

pkgReason = undefined

pkgLicenses = undefined

pkgGroups :: (StringLike a, AlpmType a) => Package -> [a]
pkgGroups = unsafePerformIO . (toList =<<) . (castPtr <$>) . {# call get_groups #}

pkgDatabase :: Package -> Database
pkgDatabase = unsafePerformIO . {# call get_db #}
