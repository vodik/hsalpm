{-# LANGUAGE DeriveDataTypeable #-}

module System.Alpm.Core.Error where

import Control.Exception
import Control.Monad.Error
import Data.Typeable
import System.Alpm.Internal.Alpm
import System.Alpm.Internal.Types

data AlpmError = Library ErrorCode String
               | Custom String
               | UnknownError String
               deriving (Eq, Typeable)

instance Exception AlpmError

instance Show AlpmError where
    show (Library n s) = s ++ ": " ++ strerror n
    show (Custom s)    = s

instance Error AlpmError where
    noMsg  = Custom "An error occured"
    strMsg = Custom
