{-# LANGUAGE CPP #-}

module Alpm.List where

import Control.Applicative
import System.IO.Unsafe

import Foreign.C
import Foreign.Ptr
import Foreign.Storable

import Alpm.Util
#include <alpm_list.h>

data AlpmList a = AlpmList
    { dataPtr :: Ptr a
    , next    :: Ptr (AlpmList a)
    }

#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)
instance Storable (AlpmList a) where
    alignment _ = #{alignment alpm_list_t}
    sizeOf _    = #{size alpm_list_t}
    peek ptr    = AlpmList <$> #{peek alpm_list_t, data} ptr
                           <*> #{peek alpm_list_t, next} ptr

integrate box ptr
    | isNull ptr = []
    | otherwise  = let (AlpmList d n) = unsafePerformIO (peek ptr) in
        box d : integrate box n
