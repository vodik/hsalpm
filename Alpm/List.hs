{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}

module Alpm.List where

import System.IO.Unsafe
import Foreign.C
import Foreign.Ptr

import Debug.Trace
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

mSort' :: (Ptr a -> b) -> (b -> b -> Ordering) -> Ptr a -> Ptr a -> CInt
mSort' box comp p1 p2 =
    case comp (box p1) (box p2) of
       LT -> 1
       EQ -> 0
       GT -> -1

mSort :: Ptr AlpmList -> SortFunc a -> Ptr AlpmList
mSort ptr sorter = unsafePerformIO $ do
    wSort <- wrap sorter
    let len = c_alpm_list_count ptr
    return $ c_alpm_list_msort ptr len wSort

-- mSort db box comp
