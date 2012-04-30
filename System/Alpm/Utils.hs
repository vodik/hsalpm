module System.Alpm.Utils where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Data.Bits
import Data.Time
import Data.Time.Clock.POSIX
import Foreign.C
import Foreign.Ptr

import System.Alpm.StringLike

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

toBitmap :: (Enum a, Num b) => [a] -> b
toBitmap = fromIntegral . foldr ((.|.) . fromEnum) 0

fromBitmap :: (Num a, Enum b) => a -> [b]
fromBitmap = undefined
