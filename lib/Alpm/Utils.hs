module Alpm.Utils where

import Control.Monad
import Control.Monad.Trans
import Data.Time
import Data.Time.Clock.POSIX
import Foreign.C
import Foreign.Ptr

toString :: MonadIO m => IO CString -> m String
toString = liftIO . (peekCString =<<)

maybeString :: MonadIO m => IO CString -> m (Maybe String)
maybeString str = liftIO $ do
    ptr <- str
    if (ptr == nullPtr)
        then liftM Just $ peekCString ptr
        else return Nothing

toDate :: MonadIO m => IO CLong -> m UTCTime
toDate = liftIO . fmap (posixSecondsToUTCTime . realToFrac)
