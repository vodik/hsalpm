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
import Alpm.Utils

{# import Alpm.Internal.Types #}

#include <alpm.h>

pkgFilename :: Package -> (Maybe String)
pkgFilename = unsafePerformIO . maybeString . {# call get_filename #}

pkgName :: Package -> String
pkgName = unsafePerformIO . toString . {# call get_name #}

pkgVersion :: Package -> String
pkgVersion = unsafePerformIO . toString . {# call get_version #}

pkgOrigin = undefined

pkgDescription :: Package -> String
pkgDescription = unsafePerformIO . toString . {# call get_desc #}

pkgURL :: Package -> String
pkgURL = unsafePerformIO . toString . {# call get_url #}

pkgBuildDate :: Package -> UTCTime
pkgBuildDate = unsafePerformIO . toDate . {# call get_builddate #}

pkgInstallDate :: Package -> UTCTime
pkgInstallDate = unsafePerformIO . toDate . {# call get_installdate #}

pkgPackager :: Package -> String
pkgPackager = unsafePerformIO . toString . {# call get_packager #}

pkgMD5Sum :: Package -> (Maybe String)
pkgMD5Sum = unsafePerformIO . maybeString . {# call get_md5sum #}

pkgSHA256Sum :: Package -> (Maybe String)
pkgSHA256Sum = unsafePerformIO . maybeString . {# call get_sha256sum #}

pkgArch :: Package -> String
pkgArch = unsafePerformIO . toString . {# call get_arch #}

pkgSize = undefined

pkgInstallSize = undefined

pkgReason = undefined

pkgLicenses = undefined

pkgGroups :: Package -> [String]
pkgGroups = unsafePerformIO . (toList =<<) . (castPtr <$>) . {# call get_groups #}

pkgDatabase :: Package -> Database
pkgDatabase = unsafePerformIO . {# call get_db #}
