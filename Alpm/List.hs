{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}

module Alpm.List where

import Control.Applicative
import System.IO.Unsafe

import Foreign.C
import Foreign.Marshal.Alloc
import Foreign.Ptr

import Alpm.Util

data AlpmList

foreign import ccall "alpm_list_next"  c_alpm_list_next  :: Ptr AlpmList -> Ptr AlpmList
integrate :: (Ptr AlpmList -> b) -> Ptr AlpmList -> [b]
integrate box ptr
    | isNull ptr = []
    | otherwise  = box ptr : integrate box (c_alpm_list_next ptr)
