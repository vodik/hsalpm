module Alpm.Util where

import Control.Applicative
import Control.DeepSeq
import System.IO.Unsafe

import Foreign.C
import Foreign.Ptr (Ptr, nullPtr)

isNull :: Ptr a -> Bool
isNull = (== nullPtr)

unsafePeekCString :: CString -> String
unsafePeekCString = unsafePerformIO . peekCString

unsafeMaybeCString :: CString -> Maybe String
unsafeMaybeCString cstr
    | isNull cstr = Nothing
    | otherwise   = unsafePerformIO $ Just <$> peekCString cstr

maybeFromIntegral :: (Integral a, Num b) => a -> Maybe b
maybeFromIntegral x = if x > 0 then Just $ fromIntegral x else Nothing

($!!) :: (NFData a) => (a -> b) -> a -> b
f $!! x = x `deepseq` f x
