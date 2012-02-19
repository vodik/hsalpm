{-# LANGUAGE CPP, ForeignFunctionInterface #-}

module Alpm.List where

import Control.Applicative
import System.IO.Unsafe

import Foreign.C
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Storable
import GHC.Ptr

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

boxAlpmList :: (Ptr t -> a) -> Ptr (AlpmList t) -> [a]
boxAlpmList box ptr | isNull ptr = []
                    | otherwise  = let (AlpmList d n _) = unsafePerformIO (peek ptr)
                                   in box d : boxAlpmList box n

foreign import ccall "&alpm_list_free" c_alpm_list_free :: FinalizerPtr (AlpmList t)

-- pokeAlpmList :: (a -> Ptr t) -> [a] -> ForeignPtr (AlpmList t)
-- mkAlpmList :: (a -> IO (Ptr t)) -> [a] -> IO (Ptr (AlpmList t))
mkAlpmList unbox x = unsafePerformIO $ do
    node <- marshal unbox x nullPtr
    return $ newForeignPtr c_alpm_list_free node

marshal unbox [] prev = do
    node <- malloc
    poke node (AlpmList nullPtr nullPtr prev)
    return node

marshal unbox (x:xs) prev = do
    node <- malloc
    poke node (AlpmList (unsafePerformIO $ unbox x) (unsafePerformIO $ marshal unbox xs node) prev)
    return node
