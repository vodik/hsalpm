{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}

module Alpm.Package where

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
    }

unsafePeekCString :: CString -> String
unsafePeekCString = unsafePerformIO . peekCString

foreign import ccall "alpm_pkg_get_name"     c_alpm_get_name     :: Ptr PkgHandle -> CString
foreign import ccall "alpm_pkg_get_version"  c_alpm_get_version  :: Ptr PkgHandle -> CString
foreign import ccall "alpm_pkg_get_desc"     c_alpm_get_desc     :: Ptr PkgHandle -> CString
foreign import ccall "alpm_pkg_get_url"      c_alpm_get_url      :: Ptr PkgHandle -> CString
foreign import ccall "alpm_pkg_get_packager" c_alpm_get_packager :: Ptr PkgHandle -> CString
foreign import ccall "alpm_pkg_get_arch"     c_alpm_get_arch     :: Ptr PkgHandle -> CString
foreign import ccall "alpm_pkg_get_size"     c_alpm_get_size     :: Ptr PkgHandle -> CSize

foreign import ccall "alpm_list_getdata" c_alpm_list_getdata :: Ptr AlpmList -> Ptr b
mkPackage :: Ptr AlpmList -> Package
mkPackage ptr = let pkg_ptr = c_alpm_list_getdata ptr
    in Package
        { packageName        = unsafePeekCString $ c_alpm_get_name pkg_ptr
        , packageVersion     = unsafePeekCString $ c_alpm_get_version pkg_ptr
        , packageDescription = unsafePeekCString $ c_alpm_get_desc pkg_ptr
        , packageURL         = unsafePeekCString $ c_alpm_get_url pkg_ptr
        , packagePackager    = unsafePeekCString $ c_alpm_get_packager pkg_ptr
        , packageArch        = unsafePeekCString $ c_alpm_get_arch pkg_ptr
        }
