{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Alpm.StringLike where

import Foreign.C.String
import Foreign.Ptr
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as E

class StringLike a where
    toC        :: a -> IO CString
    fromC      :: CString -> IO a
    toString   :: a -> String
    fromString :: String -> a

instance StringLike String where
    toC        = newCString
    fromC      = peekCString
    toString   = id
    fromString = id

instance StringLike BS.ByteString where
    toC        = newCString . show
    fromC      = BS.packCString
    toString   = BS.unpack
    fromString = BS.pack

instance StringLike T.Text where
    toC        = toC . E.encodeUtf8
    fromC      = fmap E.decodeUtf8 . fromC
    toString   = T.unpack
    fromString = T.pack
