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

data AlpmList a = AlpmList (Ptr a) (Ptr (AlpmList a)) (Ptr (AlpmList a))

instance Storable (AlpmList a) where
    alignment _ = {# alignof alpm_list_t #}
    sizeOf _    = {# sizeof  alpm_list_t #}
    peek ptr    = AlpmList <$> {# get alpm_list_t->data #} ptr
                           <*> {# get alpm_list_t->prev #} ptr
                           <*> {# get alpm_list_t->next #} ptr
    poke ptr (AlpmList d p n) = do
        {# set alpm_list_t->data #} ptr d
        {# set alpm_list_t->prev #} ptr p
        {# set alpm_list_t->next #} ptr n

unpackAlpmList :: (AlpmType a t) => Ptr (AlpmList t) -> [a]
unpackAlpmList ptr | isNull ptr = []
                   | otherwise  = let (AlpmList d n _) = unsafePerformIO (peek ptr)
                                  in unpack d : unpackAlpmList n

foreign import ccall "&alpm_list_free" c_alpm_list_free :: FinalizerPtr (AlpmList t)

packAlpmList :: (AlpmType a t) => [a] -> ForeignPtr (AlpmList t)
packAlpmList [] = unsafePerformIO . newForeignPtr c_alpm_list_free $ nullPtr
packAlpmList x  = unsafePerformIO . newForeignPtr c_alpm_list_free $ marshal x nullPtr
  where
    marshal (x:[]) p = unsafePerformIO $ malloc >>= \n -> poke n (AlpmList (pack x) nullPtr p)        >> return n
    marshal (x:xs) p = unsafePerformIO $ malloc >>= \n -> poke n (AlpmList (pack x) (marshal xs n) p) >> return n
