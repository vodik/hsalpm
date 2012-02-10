module Alpm.Util where

import Foreign.Ptr (Ptr, nullPtr)

isNull :: Ptr a -> Bool
isNull = (== nullPtr)
