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
-- foreign import ccall "alpm_list_count" c_alpm_list_count :: Ptr AlpmList -> CSize
-- foreign import ccall "alpm_list_msort" c_alpm_list_msort :: Ptr AlpmList -> CSize -> FunPtr (CompareFunc a) -> Ptr AlpmList

integrate :: (Ptr AlpmList -> b) -> Ptr AlpmList -> [b]
integrate box ptr
    | isNull ptr = []
    | otherwise  = box ptr : integrate box (c_alpm_list_next ptr)

-- type CompareFunc a = Ptr a -> Ptr a -> CInt

-- foreign import ccall "wrapper" wrap :: CompareFunc a -> IO (FunPtr (CompareFunc a))

-- mkSorter :: (Ptr a -> b) -> (b -> b -> Ordering) -> CompareFunc a
-- mkSorter box comp p1 p2 = orderingToC $ comp (box p1) (box p2)

-- sortBy :: Ptr AlpmList -> CompareFunc a -> Ptr AlpmList
-- sortBy ptr sorter = unsafePerformIO $ c_alpm_list_msort ptr (c_alpm_list_count ptr) <$> wrap sorter
