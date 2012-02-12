{-# LANGUAGE ForeignFunctionInterface #-}

module Alpm.Database where

import Control.Applicative
import Foreign.C
import Foreign.Ptr (Ptr, nullPtr)

import Alpm.Base
import Alpm.List
import Alpm.Package
import Alpm.Util

data DBHandle

data DB = DB !(Ptr DBHandle)

foreign import ccall "alpm_option_get_localdb" c_alpm_option_get_localdb :: Ptr AlpmHandle -> IO (Ptr DBHandle)
localDB :: Alpm DB
localDB = withAlpmPtr $ \alpm_ptr -> do
    db_ptr <- c_alpm_option_get_localdb alpm_ptr
    if isNull db_ptr
        then fail "could not register 'local' database"
        else return $ DB db_ptr

foreign import ccall "alpm_option_get_syncdbs" c_alpm_option_get_syncdbs :: Ptr AlpmHandle -> IO (Ptr AlpmList)
syncDBs :: Alpm [DB]
syncDBs = withAlpmPtr $ \alpm_ptr -> do
    list_ptr <- c_alpm_option_get_syncdbs alpm_ptr
    if isNull list_ptr
        then return []
        else return . integrate DB $ list_ptr

-- foreign input ccall "alpm_db_register_sync" c_alpm_db_register_sync :: Ptr AlpmHandle -> CString -> IO (Ptr DBHandle)
-- syncDB :: String -> Alpm DB
-- syncDB name = withAlpmPtr $ \alpm_ptr -> do
--     name'  <- newCString name
--     db_ptr <- c_alpm_db_register_sync alpm_ptr name'
--     if isNull db_ptr
--         then fail "could not register 'local' database"
--         else return $ DB db_ptr

foreign import ccall "alpm_db_get_pkgcache" c_alpm_db_get_pkgcache :: Ptr DBHandle -> Ptr AlpmList
packages :: DB -> [Package]
packages (DB db_ptr) = integrate mkPackage $ c_alpm_db_get_pkgcache db_ptr
