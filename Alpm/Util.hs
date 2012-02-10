module Alpm.Util where

import System.IO.Unsafe
import Foreign.C
import Foreign.Ptr (Ptr, nullPtr)

foreign import ccall "alpm_strerror" c_alpm_strerror :: CInt -> CString
alpmStrerror errno = unsafePerformIO . peekCString $ c_alpm_strerror errno

isNull :: Ptr a -> Bool
isNull = (== nullPtr)
