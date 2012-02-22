{-# LANGUAGE CPP, ForeignFunctionInterface #-}

module Alpm.List where

import Control.Applicative
import System.IO.Unsafe

import Foreign.C
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Storable

import Alpm.Base
import Alpm.Util
#include <alpm_list.h>

data AlpmList a = AlpmList
    { dataPtr :: Ptr a
    , next    :: Ptr (AlpmList a)
    , prev    :: Ptr (AlpmList a)
    }

#let alignment t = "%lu", (unsigned long)offsetof(struct { char x__; t (y__); }, y__)
instance Storable (AlpmList a) where
    alignment _ = #{alignment alpm_list_t}
    sizeOf _    = #{size alpm_list_t}
    peek ptr    = AlpmList <$> #{peek alpm_list_t, data} ptr
                           <*> #{peek alpm_list_t, next} ptr
                           <*> #{peek alpm_list_t, prev} ptr
    poke ptr (AlpmList d n p) = #{poke alpm_list_t, data} ptr d
                             >> #{poke alpm_list_t, next} ptr n
                             >> #{poke alpm_list_t, prev} ptr p

packAlpmList :: (AlpmType a t) => Ptr (AlpmList t) -> [a]
packAlpmList ptr | isNull ptr = []
                 | otherwise  = let (AlpmList d n _) = unsafePerformIO (peek ptr)
                                in pack d : packAlpmList n

foreign import ccall "&alpm_list_free" c_alpm_list_free :: FinalizerPtr (AlpmList t)

unpackAlpmList :: (AlpmType a t) => [a] -> ForeignPtr (AlpmList t)
unpackAlpmList [] = unsafePerformIO . newForeignPtr c_alpm_list_free $ nullPtr
unpackAlpmList x  = unsafePerformIO . newForeignPtr c_alpm_list_free $ marshal x nullPtr
  where
    marshal (x:[]) p = unsafePerformIO $ malloc >>= \n -> poke n (AlpmList (unpack x) nullPtr p)        >> return n
    marshal (x:xs) p = unsafePerformIO $ malloc >>= \n -> poke n (AlpmList (unpack x) (marshal xs n) p) >> return n
