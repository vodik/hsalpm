{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls, MultiParamTypeClasses #-}

module Alpm.Database where

import Control.Applicative
import Control.DeepSeq
import Control.Monad
import Control.Monad.Trans (liftIO)
import Data.Bits
import Text.Printf

import Foreign.C
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Marshal.Utils

import Alpm.Base
import Alpm.List
import Alpm.Util

data DBHandle
type DBList = AlpmList DBHandle

data DB = DB !(Ptr DBHandle)

instance AlpmType DB DBHandle  where
    pack (DB ptr) = ptr
    unpack        = DB

data UpdateStatus = Updated
                  | UpToDate
                  deriving (Eq, Show)

foreign import ccall "alpm_db_get_name" c_alpm_db_get_name :: Ptr DBHandle -> CString
dbName :: DB -> String
dbName (DB db_ptr) = unsafePeekCString $ c_alpm_db_get_name db_ptr

foreign import ccall "alpm_option_get_localdb" c_alpm_option_get_localdb :: Ptr AlpmHandle -> IO (Ptr DBHandle)
localDB :: Alpm DB
localDB = do
    db_ptr <- withAlpmPtr c_alpm_option_get_localdb
    if isNull db_ptr
        then throwAlpmException "could not register 'local' database"
        else return $ DB db_ptr

foreign import ccall "alpm_option_get_syncdbs" c_alpm_option_get_syncdbs :: Ptr AlpmHandle -> IO (Ptr DBList)
syncDBs :: Alpm [DB]
syncDBs = do
    list_ptr <- withAlpmPtr c_alpm_option_get_syncdbs
    if isNull list_ptr
        then return []
        else return $ unpackAlpmList list_ptr

foreign import ccall "alpm_db_register_sync" c_alpm_db_register_sync :: Ptr AlpmHandle -> CString -> CInt -> IO (Ptr DBHandle)
registerDB :: String -> Alpm DB
registerDB name = do
    db_ptr <- withAlpmPtr $ \alpm_ptr -> do
        name' <- newCString name
        c_alpm_db_register_sync alpm_ptr name' (1 `shiftL` 31)
    if isNull db_ptr
        then throwAlpmException $ printf "could not register '%s' database" name
        else return $ DB db_ptr


foreign import ccall "alpm_db_unregister" c_alpm_db_unregister :: Ptr DBHandle -> IO CInt
unregisterDB :: DB -> Alpm ()
unregisterDB (DB db_ptr) = do
    rst <- liftIO $ toBool <$> c_alpm_db_unregister db_ptr
    when rst . throwAlpmException $ printf "could not unregister database"

foreign import ccall "alpm_db_unregister_all" c_alpm_db_unregister_all :: IO CInt
unregisterDBs :: Alpm ()
unregisterDBs = do
    rst <- liftIO $ toBool <$> c_alpm_db_unregister_all
    when rst . throwAlpmException $ printf "could not unregister all databases"

foreign import ccall "alpm_db_get_servers" c_alpm_db_get_servers :: Ptr DBHandle -> IO (Ptr (AlpmList CChar))
servers :: DB -> Alpm [String]
servers (DB db_ptr) = do
    lst <- liftIO $ c_alpm_db_get_servers db_ptr
    return $ unpackAlpmList lst

foreign import ccall "alpm_db_add_server" c_alpm_db_add_server :: Ptr DBHandle -> CString -> IO CInt
addServer :: DB -> String -> Alpm ()
addServer (DB db_ptr) url = do
    url' <- liftIO $ newCString url
    rst  <- liftIO $ toBool <$> c_alpm_db_add_server db_ptr url'
    when rst $ throwAlpmException "unable to add server"

foreign import ccall "alpm_db_remove_server" c_alpm_db_remove_server :: Ptr DBHandle -> CString -> IO CInt
removeServer :: DB -> String -> Alpm ()
removeServer (DB db_ptr) url = do
    url' <- liftIO $ newCString url
    rst  <- liftIO $ toBool <$> c_alpm_db_remove_server db_ptr url'
    when rst $ throwAlpmException "unable to remove server"

foreign import ccall "alpm_db_update" c_alpm_db_update :: CInt -> Ptr DBHandle -> IO CInt
updateDB :: Bool -> DB -> Transaction UpdateStatus
updateDB force (DB db_ptr) = Transaction $ do
    rst <- liftIO $ fromIntegral <$> c_alpm_db_update (fromBool $ not force) db_ptr
    case rst of
        0 -> return UpToDate
        n | n < 0     -> throwAlpmException "unable to update database"
          | otherwise -> return Updated

foreign import ccall "alpm_db_get_valid" c_alpm_db_get_valid :: Ptr DBHandle -> IO CInt
validateDB :: DB -> Alpm ()
validateDB (DB db_ptr) = do
    rst <- liftIO $ toBool <$> c_alpm_db_get_valid db_ptr
    when rst $ throwAlpmException "database is not valid"
