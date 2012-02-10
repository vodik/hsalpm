{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}

module Alpm.Package where

import System.IO.Unsafe

import Foreign.C
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc

data PkgHandle

data Package = Package (Ptr PkgHandle)

unsafePeekCString :: CString -> String
unsafePeekCString = unsafePerformIO . peekCString

foreign import ccall "alpm_pkg_get_name"     c_alpm_get_name     :: Ptr PkgHandle -> CString
foreign import ccall "alpm_pkg_get_version"  c_alpm_get_version  :: Ptr PkgHandle -> CString
foreign import ccall "alpm_pkg_get_desc"     c_alpm_get_desc     :: Ptr PkgHandle -> CString
foreign import ccall "alpm_pkg_get_url"      c_alpm_get_url      :: Ptr PkgHandle -> CString
foreign import ccall "alpm_pkg_get_packager" c_alpm_get_packager :: Ptr PkgHandle -> CString
foreign import ccall "alpm_pkg_get_arch"     c_alpm_get_arch     :: Ptr PkgHandle -> CString

foreign import ccall "alpm_pkg_get_size"     c_alpm_get_size     :: Ptr PkgHandle -> CSize

packageName :: Package -> String
packageName (Package pkg_ptr) = unsafePeekCString $ c_alpm_get_name pkg_ptr

packageVersion :: Package -> String
packageVersion (Package pkg_ptr) = unsafePeekCString $ c_alpm_get_version pkg_ptr

packageDescription :: Package -> String
packageDescription (Package pkg_ptr) = unsafePeekCString $ c_alpm_get_desc pkg_ptr

packageURL :: Package -> String
packageURL (Package pkg_ptr) = unsafePeekCString $ c_alpm_get_url pkg_ptr

packagePackager :: Package -> String
packagePackager (Package pkg_ptr) = unsafePeekCString $ c_alpm_get_packager pkg_ptr

packageArch :: Package -> String
packageArch (Package pkg_ptr) = unsafePeekCString $ c_alpm_get_arch pkg_ptr

packageSize :: Package -> Int
packageSize (Package pkg_ptr) = fromIntegral $ c_alpm_get_size pkg_ptr
