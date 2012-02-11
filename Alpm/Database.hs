{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}

module Alpm.Database where

import Control.Applicative
import Foreign.C
import Foreign.Ptr (Ptr, nullPtr)

import Alpm.Base
import Alpm.List
import Alpm.Package
import Alpm.Util

data DB = DB !(Ptr AlpmList)

foreign import ccall "alpm_option_get_localdb" c_alpm_option_get_localdb :: Ptr AlpmHandle -> IO (Ptr AlpmList)
localDB :: Alpm DB
localDB = withAlpmPtr $ \alpm_ptr -> do
    db_ptr <- c_alpm_option_get_localdb alpm_ptr
    if isNull db_ptr
        then fail "could not register 'local' database"
        else return $ DB db_ptr

foreign import ccall "alpm_db_get_pkgcache" c_alpm_db_get_pkgcache :: Ptr AlpmList -> Ptr AlpmList
packages :: DB -> [Package]
packages (DB db_ptr) = integrate mkPackage $ c_alpm_db_get_pkgcache db_ptr
