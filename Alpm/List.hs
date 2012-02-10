{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}

module Alpm.List where

import System.IO.Unsafe
import Foreign.C
import Foreign.Ptr

import Debug.Trace
import Alpm.Util

data AlpmList

foreign import ccall "alpm_list_next"  c_alpm_list_next  :: Ptr AlpmList -> Ptr AlpmList
foreign import ccall "alpm_list_count" c_alpm_list_count :: Ptr AlpmList -> CInt
foreign import ccall "alpm_list_msort" c_alpm_list_msort :: Ptr AlpmList -> CInt -> FunPtr (Ptr a -> Ptr a -> CInt) -> Ptr AlpmList

integrate :: (Ptr AlpmList -> b) -> Ptr AlpmList -> [b]
integrate box ptr
    | isNull ptr = []
    | otherwise  = box ptr : integrate box (c_alpm_list_next ptr)

type SortFunc a = Ptr a -> Ptr a -> CInt

foreign import ccall "wrapper" wrap :: SortFunc a -> IO (FunPtr (SortFunc a))

mSort' :: (Ptr a -> b) -> (b -> b -> Ordering) -> Ptr a -> Ptr a -> CInt
mSort' box comp p1 p2 = 0
    -- let p1' = box p1
    --     p2' = box p2
    -- case trace "in mSort'" $ comp p1' p2' of
    --    LT -> -1
    --    EQ -> 0
    --    GT -> 1

simpleSort :: Ptr a -> Ptr a -> CInt
simpleSort _ _ = 1

mSort :: Ptr AlpmList -> SortFunc a -> Ptr AlpmList
mSort ptr sorter = unsafePerformIO $ do
    wSort <- wrap sorter
    return $ c_alpm_list_msort ptr (c_alpm_list_count ptr) wSort

-- mSort db box comp
