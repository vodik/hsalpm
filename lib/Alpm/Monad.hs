{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Alpm.Monad
    ( Alpm(..)
    , AlpmEnv(..)
    , AlpmException(..)
    , AlpmOptions(..)
    , defaultOptions
    , withHandle
    , throwAlpmException

    , runAlpm
    ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Error
import Control.Monad.Reader
import Data.IORef
import Foreign.ForeignPtr
import Foreign.Ptr
import qualified Data.Map as M

import Debug.Trace

import Alpm.Internal.Alpm
import Alpm.Core
import Alpm.Callbacks

runAlpm :: AlpmOptions -> Alpm a -> IO (Either AlpmException a)
runAlpm opt (Alpm f) = do
    alpmInitialize (root opt) (dbPath opt) >>= either failed run'
  where
    failed   = return . Left . AlpmException "failed to initialize alpm library"
    run' env = withForeignPtr env . const $ do
        ret <- runReaderT (runErrorT f) (AlpmEnv env)
        theScavanger env
        return ret

theScavanger :: AlpmHandle -> IO ()
theScavanger hdl = do
    traceIO "SCAVANGER"
    (toss, keep) <- M.mapEitherWithKey splitter `fmap` getCallbackTable
    traceIO $ "TOSSING: " ++ show toss
    mapM_ freeHaskellFunPtr $! M.elems toss
    traceIO $ "KEEPING: " ++ show keep
    setCallbackTable keep
  where
    splitter (CallbackID h _) v
        | h == hdl  = Left v
        | otherwise = Right v
