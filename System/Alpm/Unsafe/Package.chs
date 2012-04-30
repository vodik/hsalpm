{-# LANGUAGE ForeignFunctionInterface #-}

{# context lib="alpm" prefix="alpm_pkg" #}

module System.Alpm.Unsafe.Package where

import Control.Applicative
import Control.Monad.Trans
import Data.Time
import Foreign.C
import Foreign.Ptr
import System.IO.Unsafe

import System.Alpm.Database
import System.Alpm.Internal.List
import System.Alpm.Internal.Types
import System.Alpm.StringLike
import System.Alpm.Utils

{# import System.Alpm.Internal.Types #}

#include <alpm.h>

pkgFilename :: StringLike a => Package -> (Maybe a)
pkgFilename = unsafePerformIO . maybeString . {# call get_filename #}

pkgName :: StringLike a => Package -> a
pkgName = unsafePerformIO . readString . {# call get_name #}

pkgVersion :: StringLike a => Package -> a
pkgVersion = unsafePerformIO . readString . {# call get_version #}

pkgOrigin :: Package -> Origin
pkgOrigin = toEnum . fromIntegral . unsafePerformIO . {# call get_origin #}

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

pkgSize :: Package -> Maybe Int
pkgSize = justIf (> 0) . fromIntegral . unsafePerformIO . {# call get_size #}

pkgInstallSize :: Package -> Int
pkgInstallSize = fromIntegral . unsafePerformIO . {# call get_isize #}

pkgReason :: Package -> Reason
pkgReason = toEnum . fromIntegral . unsafePerformIO . {# call get_reason #}

pkgLicenses :: (StringLike a, AlpmType a) => Package -> [a]
pkgLicenses = unsafePerformIO . (toList =<<) . (castPtr <$>) . {# call get_licenses #}

pkgGroups :: (StringLike a, AlpmType a) => Package -> [a]
pkgGroups = unsafePerformIO . (toList =<<) . (castPtr <$>) . {# call get_groups #}

pkgDepends :: (StringLike a, AlpmType a) => Package -> [a]
pkgDepends = unsafePerformIO . (toList =<<) . (castPtr <$>) . {# call get_depends #}

pkgOptDepends :: (StringLike a, AlpmType a) => Package -> [a]
pkgOptDepends = unsafePerformIO . (toList =<<) . (castPtr <$>) . {# call get_optdepends #}

pkgConflicts :: (StringLike a, AlpmType a) => Package -> [a]
pkgConflicts = unsafePerformIO . (toList =<<) . (castPtr <$>) . {# call get_conflicts #}

pkgProvides :: (StringLike a, AlpmType a) => Package -> [a]
pkgProvides = unsafePerformIO . (toList =<<) . (castPtr <$>) . {# call get_provides #}

pkgDeltas :: (StringLike a, AlpmType a) => Package -> [a]
pkgDeltas = unsafePerformIO . (toList =<<) . (castPtr <$>) . {# call get_deltas #}

pkgReplaces :: (StringLike a, AlpmType a) => Package -> [a]
pkgReplaces = unsafePerformIO . (toList =<<) . (castPtr <$>) . {# call get_replaces #}

pkgFiles :: (StringLike a, AlpmType a) => Package -> [a]
pkgFiles = undefined

pkgBackup :: (StringLike a, AlpmType a) => Package -> [a]
pkgBackup = unsafePerformIO . (toList =<<) . (castPtr <$>) . {# call get_backup #}

pkgDatabase :: Package -> Database
pkgDatabase = unsafePerformIO . {# call get_db #}

pkgBase64Sig :: StringLike a => Package -> a
pkgBase64Sig = unsafePerformIO . readString . {# call get_base64_sig #}
