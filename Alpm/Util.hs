{-# LANGUAGE ForeignFunctionInterface #-}

module Alpm.Util where

import Control.DeepSeq
import System.IO.Unsafe

import Foreign.C
import Foreign.Ptr (Ptr, nullPtr)

foreign import ccall "alpm_strerror" c_alpm_strerror :: CInt -> CString
alpmStrerror errno = unsafePerformIO . peekCString $ c_alpm_strerror errno

isNull :: Ptr a -> Bool
isNull = (== nullPtr)

whenJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenJust p f = maybe (return ()) f p

($!!) :: (NFData a) => (a -> b) -> a -> b
f $!! x = x `deepseq` f x
