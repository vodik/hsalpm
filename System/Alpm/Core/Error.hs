{-# LANGUAGE DeriveDataTypeable #-}

module System.Alpm.Core.Error where

import Control.Exception
import Control.Monad.Error
import Data.Typeable

data AlpmError = Generic String String
               | UnknownError String
               deriving (Eq, Typeable)

instance Exception AlpmError

instance Show AlpmError where
    show (Generic s v)    = s ++ ": " ++ v
    show (UnknownError s) = s

instance Error AlpmError where
    noMsg  = UnknownError "An error occured"
    strMsg = UnknownError
