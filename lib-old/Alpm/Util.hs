module Alpm.Util where

import Control.Applicative
import Control.DeepSeq
import System.IO.Unsafe

import Foreign.C
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Marshal.Utils

isNull :: Ptr a -> Bool
isNull = (== nullPtr)

unsafePeekCString :: CString -> String
unsafePeekCString = unsafePerformIO . peekCString

unsafeMaybeCString :: CString -> Maybe String
unsafeMaybeCString cstr
    | isNull cstr = Nothing
    | otherwise   = unsafePerformIO $ Just <$> peekCString cstr

maybeFromIntegral :: (Integral a, Num b) => a -> Maybe b
maybeFromIntegral x = if toBool x then Just $ fromIntegral x else Nothing

toAlpmEnum :: (Enum a) => Int -> a
toAlpmEnum = toEnum . subtract 1

fromAlpmEnum :: (Enum a) => a -> Int
fromAlpmEnum = (+ 1) . fromEnum

infixr 0 $!!
($!!) :: (NFData a) => (a -> b) -> a -> b
f $!! x = x `deepseq` f x
