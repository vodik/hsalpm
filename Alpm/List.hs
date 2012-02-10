{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}

module Alpm.List where

import Control.Applicative
import System.IO.Unsafe
import Foreign.C
import Foreign.Ptr

import Alpm.Util

data AlpmList

foreign import ccall "alpm_list_next"  c_alpm_list_next  :: Ptr AlpmList -> Ptr AlpmList
foreign import ccall "alpm_list_count" c_alpm_list_count :: Ptr AlpmList -> CSize
foreign import ccall "alpm_list_msort" c_alpm_list_msort :: Ptr AlpmList -> CSize -> FunPtr (SortFunc a) -> Ptr AlpmList

integrate :: (Ptr AlpmList -> b) -> Ptr AlpmList -> [b]
integrate box ptr
    | isNull ptr = []
    | otherwise  = box ptr : integrate box (c_alpm_list_next ptr)

type SortFunc a = Ptr a -> Ptr a -> CInt

foreign import ccall "wrapper" wrap :: SortFunc a -> IO (FunPtr (SortFunc a))

mkSorter :: (Ptr a -> b) -> (b -> b -> Ordering) -> SortFunc a
mkSorter box comp p1 p2 = orderingToC $ comp (box p1) (box p2)

sort :: Ptr AlpmList -> SortFunc a -> Ptr AlpmList
sort ptr sorter = unsafePerformIO $
    c_alpm_list_msort ptr (c_alpm_list_count ptr) <$> wrap sorter
