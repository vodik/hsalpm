module Alpm.Core.Error where

data AlpmError = Generic String
               | UnknownError String
               deriving (Eq, Typeable)

instance Exception AlpmError

instance Show AlpmError where
    show (Generic s)     = s
    show (UnkownError s) = s

instance Error MPDError where
    noMsg  = UnknownError "An error occured"
    strMsg = UnknownError
