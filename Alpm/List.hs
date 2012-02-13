{-# INCLUDE <alpm_list.h> #-}
{-# LINE 1 "Alpm/List.hsc" #-}
{-# LANGUAGE CPP #-}
{-# LINE 2 "Alpm/List.hsc" #-}

module Alpm.List where

import Control.Applicative
import System.IO.Unsafe

import Foreign.C
import Foreign.Ptr
import Foreign.Storable
import GHC.Ptr

import Alpm.Util

{-# LINE 15 "Alpm/List.hsc" #-}

data AlpmList a = AlpmList
    { dataPtr :: Ptr a
    , next    :: Ptr (AlpmList a)
    }


{-# LINE 22 "Alpm/List.hsc" #-}
instance Storable (AlpmList a) where
    alignment _ = 4
{-# LINE 24 "Alpm/List.hsc" #-}
    sizeOf _    = (12)
{-# LINE 25 "Alpm/List.hsc" #-}
    peek ptr    = AlpmList <$> (\hsc_ptr -> peekByteOff hsc_ptr 0) ptr
{-# LINE 26 "Alpm/List.hsc" #-}
                           <*> (\hsc_ptr -> peekByteOff hsc_ptr 8) ptr
{-# LINE 27 "Alpm/List.hsc" #-}

integrate box ptr
    | isNull ptr = []
    | otherwise  = let (AlpmList d n) = unsafePerformIO (peek ptr) in
        box d : integrate box n
