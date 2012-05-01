{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module System.Alpm.StringLike where

import Foreign.C.String
import Foreign.Ptr
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as E

class StringLike a where
    toC   :: a -> IO CString
    fromC :: CString -> IO a

instance StringLike String where
    toC   = newCString
    fromC = peekCString

-- TODO: toC is a hack
instance StringLike BS.ByteString where
    toC   = newCString . show
    fromC = BS.packCString

instance StringLike T.Text where
    toC   = toC . E.encodeUtf8
    fromC = fmap E.decodeUtf8 . fromC
