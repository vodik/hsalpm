module Alpm.Callbacks where

import Control.Applicative
import Control.Monad.Trans (liftIO)
import Control.Monad.Reader
import Data.IORef
import Foreign.C
import Foreign.Ptr
import System.IO.Unsafe
import qualified Data.Map as M

import Alpm.Core
import Alpm.Internal.Alpm

data CallbackType = LogCB   | DownloadCB
                  | FetchCB | TotalCB
                  | EventCB | QuestionCB
                  | ProgressCB
                  deriving (Show, Eq, Ord)

data CallbackID = CallbackID AlpmHandle CallbackType
    deriving (Show, Eq, Ord)

type CallbackTable a = M.Map CallbackID (FunPtr a)

--------------------------------------------------------------------------------
-- This seems to be a common Haskell hack nowadays: A plain old global variable
-- with an associated mutator. Perhaps some language/library support is needed?

{-# NOINLINE theCallbackTable #-}
theCallbackTable :: IORef (CallbackTable a)
theCallbackTable = unsafePerformIO $ newIORef M.empty

getCallbackTable :: IO (CallbackTable a)
getCallbackTable = readIORef theCallbackTable

setCallbackTable :: CallbackTable a -> IO ()
setCallbackTable = writeIORef theCallbackTable

modifyCallbackTable :: (CallbackTable a -> CallbackTable a) -> IO ()
modifyCallbackTable = modifyIORef theCallbackTable

--------------------------------------------------------------------------------

findCallback :: CallbackID -> IO (Maybe (FunPtr a))
findCallback = (<$> getCallbackTable) . M.lookup

deleteCallback :: CallbackID -> IO ()
deleteCallback = modifyCallbackTable . M.delete

addCallback :: CallbackID -> FunPtr a -> IO ()
addCallback callbackID = modifyCallbackTable . M.insert callbackID

--------------------------------------------------------------------------------

-- TODO: add error handling where void is
setCallback :: CallbackType -> (Ptr () -> FunPtr a -> IO CInt) -> (b -> IO (FunPtr a)) -> Maybe b -> Alpm ()
setCallback callbackType registerWith mkCallback maybeCallback = do
    handle <- asks handle
    case maybeCallback of
        Nothing -> void $ register nullFunPtr
        Just cb -> do
            ptr <- liftIO $ mkCallback cb
            liftIO $ addCallback (CallbackID handle callbackType) ptr
            void $ register ptr
  where
    register = withHandle . flip registerWith
