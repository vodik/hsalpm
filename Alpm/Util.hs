module Alpm.Util where

import Control.DeepSeq
import System.IO.Unsafe
import Foreign.C
import Foreign.Ptr (Ptr, nullPtr)

foreign import ccall "alpm_strerror" c_alpm_strerror :: CInt -> CString
alpmStrerror errno = unsafePerformIO . peekCString $ c_alpm_strerror errno

isNull :: Ptr a -> Bool
isNull = (== nullPtr)

($!!) :: (NFData a) => (a -> b) -> a -> b
f $!! x = x `deepseq` f x

orderingToC :: Ordering -> CInt
orderingToC LT = 1
orderingToC EQ = 0
orderingToC GT = -1
