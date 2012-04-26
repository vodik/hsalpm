module Alpm.Utils where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Data.Bits
import Data.Time
import Data.Time.Clock.POSIX
import Foreign.C
import Foreign.Ptr

toString :: MonadIO m => IO CString -> m String
toString = liftIO . (peekCString =<<)

maybeString :: MonadIO m => IO CString -> m (Maybe String)
maybeString str = liftIO $ do
    ptr <- str
    if ptr /= nullPtr
        then Just <$> peekCString ptr
        else return Nothing

toDate :: (MonadIO m, Real a) => IO a -> m UTCTime
toDate = liftIO . fmap (posixSecondsToUTCTime . realToFrac)

toBitmap :: Enum a => [a] -> CInt
toBitmap = fromIntegral . foldr ((.|.) . fromEnum) 0
