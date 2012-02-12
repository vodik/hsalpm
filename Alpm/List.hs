{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}

module Alpm.List where

import Control.Applicative
import System.IO.Unsafe

import Foreign.C
import Foreign.Ptr (Ptr, nullPtr)

import Alpm.Util

data AlpmList

foreign import ccall "alpm_list_next"    c_alpm_list_next    :: Ptr AlpmList -> Ptr AlpmList
foreign import ccall "alpm_list_getdata" c_alpm_list_getdata :: Ptr AlpmList -> Ptr a

integrate :: (Ptr a -> b) -> Ptr AlpmList -> [b]
integrate box ptr
    | isNull ptr = []
    | otherwise  = boxWith ptr : integrate box (c_alpm_list_next ptr)
  where
    boxWith = box . c_alpm_list_getdata
