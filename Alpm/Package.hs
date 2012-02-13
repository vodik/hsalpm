{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}

module Alpm.Package where

import Control.DeepSeq
import Data.Function
import Data.Time
import Data.Time.Clock.POSIX
import System.IO.Unsafe

import Foreign.C
import Foreign.Ptr (Ptr, nullPtr)

import Alpm.Base
import Alpm.Database
import Alpm.List
import Alpm.Util

type Arch = String
type Group = String
type License = String

data PkgHandle

data Origin = File | LocalDB | SyncDB
    deriving (Eq, Show, Read, Enum)

data Reason = Explicit | Dependent
    deriving (Eq, Show, Read, Enum)

data Package = Package
    { packageFilename    :: Maybe String
    , packageName        :: String
    , packageVersion     :: String
    , packageOrigin      :: Origin
    , packageDescription :: String
    , packageURL         :: String
    , packageBuildDate   :: UTCTime
    , packageInstallDate :: UTCTime
    , packagePackager    :: String
    , packageMD5Sum      :: Maybe String
    , packageSHA256Sum   :: Maybe String
    , packageArch        :: Arch
    , packageSize        :: Maybe Integer
    , packageInstallSize :: Integer
    , packageReason      :: Reason
    , packageLicenses    :: [License]
    , packageGroups      :: [Group]
    , packageDB          :: String
    }
    deriving (Eq, Show)

instance NFData Package where
    rnf p = packageFilename p
      `seq` packageName p
      `seq` packageVersion p
      `seq` packageOrigin p
      `seq` packageDescription p
      `seq` packageURL p
      `seq` packageBuildDate p
      `seq` packageInstallDate p
      `seq` packagePackager p
      `seq` packageMD5Sum p
      `seq` packageSHA256Sum p
      `seq` packageArch p
      `seq` packageSize p
      `seq` packageInstallSize p
      `seq` packageLicenses p
      `seq` packageGroups p
      `seq` packageDB p
      `seq` ()

foreign import ccall "alpm_db_get_pkgcache" c_alpm_db_get_pkgcache :: Ptr DBHandle -> Ptr AlpmList
packages (DB db_ptr) = integrate mkPackage $ c_alpm_db_get_pkgcache db_ptr

withPackages :: (NFData a) => ([Package] -> Alpm [a]) -> DB -> Alpm [a]
withPackages f db = f (packages db) >>= (return $!!)

withPackages_ :: ([Package] -> Alpm ()) -> DB -> Alpm ()
withPackages_ f db = f $ packages db

foreign import ccall "alpm_pkg_get_filename"    c_alpm_pkg_get_filename    :: Ptr PkgHandle -> CString
foreign import ccall "alpm_pkg_get_name"        c_alpm_pkg_get_name        :: Ptr PkgHandle -> CString
foreign import ccall "alpm_pkg_get_version"     c_alpm_pkg_get_version     :: Ptr PkgHandle -> CString
foreign import ccall "alpm_pkg_get_origin"      c_alpm_pkg_get_origin      :: Ptr PkgHandle -> CInt
foreign import ccall "alpm_pkg_get_desc"        c_alpm_pkg_get_desc        :: Ptr PkgHandle -> CString
foreign import ccall "alpm_pkg_get_url"         c_alpm_pkg_get_url         :: Ptr PkgHandle -> CString
foreign import ccall "alpm_pkg_get_builddate"   c_alpm_pkg_get_builddate   :: Ptr PkgHandle -> CTime
foreign import ccall "alpm_pkg_get_installdate" c_alpm_pkg_get_installdate :: Ptr PkgHandle -> CTime
foreign import ccall "alpm_pkg_get_packager"    c_alpm_pkg_get_packager    :: Ptr PkgHandle -> CString
foreign import ccall "alpm_pkg_get_md5sum"      c_alpm_pkg_get_md5sum      :: Ptr PkgHandle -> CString
foreign import ccall "alpm_pkg_get_sha256sum"   c_alpm_pkg_get_sha256sum   :: Ptr PkgHandle -> CString
foreign import ccall "alpm_pkg_get_arch"        c_alpm_pkg_get_arch        :: Ptr PkgHandle -> CString
foreign import ccall "alpm_pkg_get_size"        c_alpm_pkg_get_size        :: Ptr PkgHandle -> CSize
foreign import ccall "alpm_pkg_get_isize"       c_alpm_pkg_get_isize       :: Ptr PkgHandle -> CSize
foreign import ccall "alpm_pkg_get_reason"      c_alpm_pkg_get_reason      :: Ptr PkgHandle -> CInt
foreign import ccall "alpm_pkg_get_licenses"    c_alpm_pkg_get_licenses    :: Ptr PkgHandle -> Ptr AlpmList
foreign import ccall "alpm_pkg_get_groups"      c_alpm_pkg_get_groups      :: Ptr PkgHandle -> Ptr AlpmList
-- TODO: depends
-- TODO: optdepends
-- TODO: conflics
-- TODO: provides
-- TODO: deltas
-- TODO: replaces
-- TODO: files
-- TODO: backups
foreign import ccall "alpm_pkg_get_db"          c_alpm_pkg_get_db          :: Ptr PkgHandle -> Ptr a
-- TODO: sig

mkPackage :: Ptr PkgHandle -> Package
mkPackage ptr = Package
    { packageFilename    = unsafeMaybeCString $ c_alpm_pkg_get_filename ptr
    , packageName        = unsafePeekCString $ c_alpm_pkg_get_name ptr
    , packageVersion     = unsafePeekCString $ c_alpm_pkg_get_version ptr
    , packageOrigin      = toEnum . subtract 1 . fromIntegral $ c_alpm_pkg_get_origin ptr
    , packageDescription = unsafePeekCString $ c_alpm_pkg_get_desc ptr
    , packageURL         = unsafePeekCString $ c_alpm_pkg_get_url ptr
    , packageBuildDate   = posixSecondsToUTCTime . realToFrac $ c_alpm_pkg_get_builddate ptr
    , packageInstallDate = posixSecondsToUTCTime . realToFrac $ c_alpm_pkg_get_installdate ptr
    , packagePackager    = unsafePeekCString $ c_alpm_pkg_get_packager ptr
    , packageMD5Sum      = unsafeMaybeCString $ c_alpm_pkg_get_md5sum ptr
    , packageSHA256Sum   = unsafeMaybeCString $ c_alpm_pkg_get_sha256sum ptr
    , packageArch        = unsafePeekCString $ c_alpm_pkg_get_arch ptr
    , packageSize        = maybeFromIntegral $ c_alpm_pkg_get_size ptr
    , packageInstallSize = fromIntegral $ c_alpm_pkg_get_isize ptr
    , packageReason      = toEnum . fromIntegral $ c_alpm_pkg_get_reason ptr
    , packageLicenses    = integrate unsafePeekCString $ c_alpm_pkg_get_licenses ptr
    , packageGroups      = integrate unsafePeekCString $ c_alpm_pkg_get_groups ptr
    , packageDB          = dbName . DB $ c_alpm_pkg_get_db ptr
    }

byInstallSize :: Package -> Package -> Ordering
byInstallSize = compare `on` packageInstallSize

bySize :: Package -> Package -> Ordering
bySize = compare `on` packageSize
