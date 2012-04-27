module Alpm.Utils where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Data.Bits
import Data.Time
import Data.Time.Clock.POSIX
import Foreign.C
import Foreign.Ptr

import Alpm.StringLike

readString :: (MonadIO m, StringLike a) => IO CString -> m a
readString = liftIO . (fromC =<<)

maybeString :: (MonadIO m, StringLike a) => IO CString -> m (Maybe a)
maybeString str = liftIO $ do
    ptr <- str
    if ptr /= nullPtr
        then Just <$> fromC ptr
        else return Nothing

toDate :: (MonadIO m, Integral a) => IO a -> m UTCTime
toDate = liftIO . fmap (posixSecondsToUTCTime . fromIntegral)

justIf :: (a -> Bool) -> a -> Maybe a
justIf cond x = if cond x then Just x else Nothing

toBitfield :: (Enum a, Bits b) => [a] -> b
toBitfield = fromIntegral . foldr ((.|.) . fromEnum) 0

fromBitfield :: (Bits a, Enum b) => a -> [b]
fromBitfield n = go 1 n
  where go a n
          | n == 0    = []
          | isSet n 0 = toEnum a : go (a `shiftL` 1) (n `shiftR` 1)
          | otherwise = go (a `shiftL` 1) (n `shiftR` 1)

isSet :: Bits a => a -> Int -> Bool
isSet = (1 ==) .: (. bit) . (.&.)

infixl 8 .:
(.:) = (.) . (.)
