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

toDate :: MonadIO m => IO CLong -> m UTCTime
toDate = liftIO . fmap (posixSecondsToUTCTime . realToFrac)

toBitmap :: Enum a => [a] -> CInt
toBitmap = fromIntegral . foldr ((.|.) . fromEnum) 0
