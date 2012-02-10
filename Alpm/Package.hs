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

foreign import ccall "alpm_pkg_get_name"    c_alpm_get_name    :: Ptr a -> CString
foreign import ccall "alpm_pkg_get_version" c_alpm_get_version :: Ptr a -> CString
foreign import ccall "alpm_pkg_get_desc"    c_alpm_get_desc    :: Ptr a -> CString
foreign import ccall "alpm_pkg_get_url"     c_alpm_get_url     :: Ptr a -> CString
foreign import ccall "alpm_pkg_get_size"    c_alpm_get_size    :: Ptr a -> CSize

packageName :: Package -> String
packageName (Package pkg_ptr) = unsafePeekCString $ c_alpm_get_name pkg_ptr

packageVersion :: Package -> String
packageVersion (Package pkg_ptr) = unsafePeekCString $ c_alpm_get_version pkg_ptr

packageDescription :: Package -> String
packageDescription (Package pkg_ptr) = unsafePeekCString $ c_alpm_get_desc pkg_ptr

packageURL :: Package -> String
packageURL (Package pkg_ptr) = unsafePeekCString $ c_alpm_get_url pkg_ptr

packageSize :: Package -> Int
packageSize (Package pkg_ptr) = fromIntegral $ c_alpm_get_size pkg_ptr
