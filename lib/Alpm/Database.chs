{-# LANGUAGE ForeignFunctionInterface #-}

{# context lib="alpm" prefix="alpm" #}

module Alpm.Database where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Data.Bits

import Foreign.C
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Storable

import Alpm.Core
import Alpm.Internal.List
import Alpm.Internal.Types

#include <alpm.h>

{# pointer *alpm_db_t as Database newtype #}

instance AlpmType Database where
    unpack (Database ptr) = return ptr
    pack = return . Database

dbName :: Database -> Alpm String
dbName = valueToString . {# call db_get_name #}

localDB :: Alpm Database
localDB = withHandle {# call get_localdb #}

syncDBs :: Alpm [Database]
syncDBs = withHandle $ (toList =<<) . (castPtr <$>) . {# call get_syncdbs #}

registerDB :: String -> [SignatureLevel] -> Alpm Database
registerDB name sig = do
    withHandle $ \h -> do
        name' <- newCString name
        {# call register_syncdb #} h name' (sigLevel sig)

unregisterDB :: Database -> Alpm ()
unregisterDB = liftIO . void . {# call db_unregister #}

unregisterDBs :: Alpm ()
unregisterDBs = void $ withHandle {# call unregister_all_syncdbs #}

valueToString :: IO CString -> Alpm String
valueToString = liftIO . (peekCString =<<)
