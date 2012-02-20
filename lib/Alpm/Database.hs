{-# LANGUAGE EmptyDataDecls, ForeignFunctionInterface #-}

module Alpm.Database where

import Control.Applicative
import Control.DeepSeq
import Control.Monad
import Control.Monad.Trans (liftIO)
import Data.Bits
import Text.Printf

import Foreign.C
import Foreign.Ptr (Ptr, nullPtr)

import Alpm.Base
import Alpm.List
import Alpm.Util

data DBHandle
type DBList = AlpmList DBHandle

data DB = DB !(Ptr DBHandle)

foreign import ccall "alpm_db_get_name" c_alpm_db_get_name :: Ptr DBHandle -> CString
dbName :: DB -> String
dbName (DB db_ptr) = unsafePeekCString $ c_alpm_db_get_name db_ptr

foreign import ccall "alpm_option_get_localdb" c_alpm_option_get_localdb :: Ptr AlpmHandle -> IO (Ptr DBHandle)
localDB :: Alpm DB
localDB = do
    db_ptr <- withAlpmPtr $ c_alpm_option_get_localdb
    if isNull db_ptr
        then throwAlpmException "could not register 'local' database"
        else return $ DB db_ptr

foreign import ccall "alpm_option_get_syncdbs" c_alpm_option_get_syncdbs :: Ptr AlpmHandle -> IO (Ptr DBList)
syncDBs :: Alpm [DB]
syncDBs = do
    list_ptr <- withAlpmPtr $ c_alpm_option_get_syncdbs
    if isNull list_ptr
        then return []
        else return $ boxAlpmList DB list_ptr

foreign import ccall "alpm_db_register_sync" c_alpm_db_register_sync :: Ptr AlpmHandle -> CString -> CInt -> IO (Ptr DBHandle)
registerDB :: String -> Alpm DB
registerDB name = do
    db_ptr <- withAlpmPtr $ \alpm_ptr -> do
        name'  <- newCString name
        c_alpm_db_register_sync alpm_ptr name' (1 `shiftL` 31)
    if isNull db_ptr
        then throwAlpmException $ "could not register '" ++ name ++ "' database"
        else return $ DB db_ptr

foreign import ccall "alpm_db_get_servers" c_alpm_db_get_servers :: Ptr DBHandle -> IO (Ptr (AlpmList CChar))
servers :: DB -> Alpm [String]
servers (DB db_ptr) = do
    lst <- liftIO $ c_alpm_db_get_servers db_ptr
    return $ boxAlpmList unsafePeekCString lst

foreign import ccall "alpm_db_add_server" c_alpm_db_add_server :: Ptr DBHandle -> CString -> IO CInt
addServer :: DB -> String -> Alpm ()
addServer (DB db_ptr) url = do
    url' <- liftIO $ newCString url
    rst  <- liftIO $ c_alpm_db_add_server db_ptr url'
    when (rst < 0) $ throwAlpmException "Unable to add server"

foreign import ccall "alpm_db_update" c_alpm_db_update :: CInt -> Ptr DBHandle -> IO CInt
updateDB :: Bool -> DB -> Transaction Int
updateDB force (DB db_ptr) = Transaction $ do
    let f = if force then 1 else 0
    rst <- liftIO $ c_alpm_db_update f db_ptr
    if rst < 0
        then throwAlpmException "Unable to update database"
        else return $ fromIntegral rst
