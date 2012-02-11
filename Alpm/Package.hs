{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}

module Alpm.Package where

import Control.DeepSeq
import Data.Function
import Data.Time
import Data.Time.Clock.POSIX
import System.IO.Unsafe

import Foreign.C
import Foreign.Ptr (Ptr, nullPtr)

import Alpm.List
import Alpm.Util

data PkgHandle

data PackageOrigin = File | LocalDB | SyncDB
    deriving (Eq, Show, Read, Enum)

data Package = Package
    { packageFilename    :: Maybe String
    , packageName        :: String
    , packageVersion     :: String
    , packageOrigin      :: PackageOrigin
    , packageDescription :: String
    , packageURL         :: String
    , packageBuildDate   :: UTCTime
    , packageInstallDate :: UTCTime
    , packagePackager    :: String
    , packageArch        :: String
    , packageSize        :: Maybe Integer
    , packageInstallSize :: Integer
    , packageGroups      :: [Group]
    }
    deriving (Eq, Show)

type Group = String

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
      `seq` packageArch p
      `seq` packageSize p
      `seq` packageInstallSize p
      `seq` packageGroups p
      `seq` ()

foreign import ccall "alpm_pkg_get_filename"    c_alpm_pkg_get_filename    :: Ptr PkgHandle -> CString
foreign import ccall "alpm_pkg_get_name"        c_alpm_pkg_get_name        :: Ptr PkgHandle -> CString
foreign import ccall "alpm_pkg_get_version"     c_alpm_pkg_get_version     :: Ptr PkgHandle -> CString
foreign import ccall "alpm_pkg_get_origin"      c_alpm_pkg_get_origin      :: Ptr PkgHandle -> CInt
foreign import ccall "alpm_pkg_get_desc"        c_alpm_pkg_get_desc        :: Ptr PkgHandle -> CString
foreign import ccall "alpm_pkg_get_url"         c_alpm_pkg_get_url         :: Ptr PkgHandle -> CString
foreign import ccall "alpm_pkg_get_builddate"   c_alpm_pkg_get_builddate   :: Ptr PkgHandle -> CTime
foreign import ccall "alpm_pkg_get_installdate" c_alpm_pkg_get_installdate :: Ptr PkgHandle -> CTime
foreign import ccall "alpm_pkg_get_packager"    c_alpm_pkg_get_packager    :: Ptr PkgHandle -> CString
-- TODO: md5
-- TODO: sha256sum
foreign import ccall "alpm_pkg_get_arch"        c_alpm_pkg_get_arch        :: Ptr PkgHandle -> CString
foreign import ccall "alpm_pkg_get_size"        c_alpm_pkg_get_size        :: Ptr PkgHandle -> CSize
foreign import ccall "alpm_pkg_get_isize"       c_alpm_pkg_get_isize       :: Ptr PkgHandle -> CSize
-- TODO: reason
-- TODO: license
foreign import ccall "alpm_pkg_get_groups"      c_alpm_pkg_get_groups      :: Ptr PkgHandle -> Ptr AlpmList
-- TODO: depends
-- TODO: optdepends
-- TODO: conflics
-- TODO: provides
-- TODO: deltas
-- TODO: replaces
-- TODO: files
-- TODO: backups
-- TODO: db
-- TODO: sig

foreign import ccall "alpm_list_getdata" c_alpm_list_getpkg :: Ptr AlpmList -> Ptr PkgHandle
mkPackage :: Ptr AlpmList -> Package
mkPackage node = let ptr = c_alpm_list_getpkg node in Package
    { packageFilename    = unsafeMaybeCString $ c_alpm_pkg_get_filename ptr
    , packageName        = unsafePeekCString $ c_alpm_pkg_get_name ptr
    , packageVersion     = unsafePeekCString $ c_alpm_pkg_get_version ptr
    , packageOrigin      = toEnum . subtract 1 . fromIntegral $ c_alpm_pkg_get_origin ptr
    , packageDescription = unsafePeekCString $ c_alpm_pkg_get_desc ptr
    , packageURL         = unsafePeekCString $ c_alpm_pkg_get_url ptr
    , packageBuildDate   = posixSecondsToUTCTime . realToFrac $ c_alpm_pkg_get_builddate ptr
    , packageInstallDate = posixSecondsToUTCTime . realToFrac $ c_alpm_pkg_get_installdate ptr
    , packagePackager    = unsafePeekCString $ c_alpm_pkg_get_packager ptr
    , packageArch        = unsafePeekCString $ c_alpm_pkg_get_arch ptr
    , packageSize        = maybeFromIntegral $ c_alpm_pkg_get_size ptr
    , packageInstallSize = fromIntegral $ c_alpm_pkg_get_isize ptr
    , packageGroups      = integrate mkStringList $ c_alpm_pkg_get_groups ptr
    }

byInstallSize :: Package -> Package -> Ordering
byInstallSize = compare `on` packageInstallSize

bySize :: Package -> Package -> Ordering
bySize = compare `on` packageSize
