{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}

module Alpm.Package where

import Data.Function
import Control.DeepSeq
import System.IO.Unsafe
import Foreign.C
import Foreign.Ptr (Ptr, nullPtr)
import Alpm.List

data PkgHandle

data Package = Package
    { packageName        :: String
    , packageVersion     :: String
    , packageDescription :: String
    , packageURL         :: String
    , packagePackager    :: String
    , packageArch        :: String
    , packageInstallSize :: Integer
    }
    deriving (Eq, Show)

instance NFData Package where
    rnf p = packageName p
      `seq` packageVersion p
      `seq` packageDescription p
      `seq` packageURL p
      `seq` packagePackager p
      `seq` packageArch p
      `seq` packageInstallSize p
      `seq` ()

unsafePeekCString :: CString -> String
unsafePeekCString = unsafePerformIO . peekCString

foreign import ccall "alpm_pkg_get_name"     c_alpm_get_name     :: Ptr PkgHandle -> CString
foreign import ccall "alpm_pkg_get_version"  c_alpm_get_version  :: Ptr PkgHandle -> CString
foreign import ccall "alpm_pkg_get_desc"     c_alpm_get_desc     :: Ptr PkgHandle -> CString
foreign import ccall "alpm_pkg_get_url"      c_alpm_get_url      :: Ptr PkgHandle -> CString
foreign import ccall "alpm_pkg_get_packager" c_alpm_get_packager :: Ptr PkgHandle -> CString
foreign import ccall "alpm_pkg_get_arch"     c_alpm_get_arch     :: Ptr PkgHandle -> CString
foreign import ccall "alpm_pkg_get_isize"    c_alpm_get_isize    :: Ptr PkgHandle -> CSize

foreign import ccall "alpm_list_getdata" c_alpm_list_getdata :: Ptr AlpmList -> Ptr PkgHandle
mkPackage :: Ptr PkgHandle -> Package
mkPackage ptr = Package
    { packageName        = unsafePeekCString $ c_alpm_get_name ptr
    , packageVersion     = unsafePeekCString $ c_alpm_get_version ptr
    , packageDescription = unsafePeekCString $ c_alpm_get_desc ptr
    , packageURL         = unsafePeekCString $ c_alpm_get_url ptr
    , packagePackager    = unsafePeekCString $ c_alpm_get_packager ptr
    , packageArch        = unsafePeekCString $ c_alpm_get_arch ptr
    , packageInstallSize = fromIntegral $ c_alpm_get_isize ptr
    }

-- bySize :: Package -> Package -> Ordering
bySize = mSort' mkPackage (compare `on` packageInstallSize)
