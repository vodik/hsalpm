{-# LANGUAGE ForeignFunctionInterface #-}

{# context lib="alpm" prefix="alpm" #}

module Alpm.Database where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Foreign.C
import Foreign.Ptr

import Alpm.Core
import Alpm.Internal.List
import Alpm.Internal.Types
import Alpm.Utils

{# import Alpm.Internal.Types #}

#include <alpm.h>

dbName :: Database -> Alpm String
dbName = toString . {# call db_get_name #}

localDB :: Alpm Database
localDB = withHandle {# call get_localdb #}

-- TODO: Low level API: toList doesn't belong here just yet.
syncDBs :: Alpm [Database]
syncDBs = withHandle $ (toList =<<) . (castPtr <$>) . {# call get_syncdbs #}

registerDB :: String -> [SignatureLevel] -> Alpm Database
registerDB name sig = do
    withHandle $ \h -> do
        name' <- newCString name
        {# call register_syncdb #} h name' (sigLevel sig)

-- TODO: replace void with error handling
unregisterDBs :: Alpm ()
unregisterDBs = void $ withHandle {# call unregister_all_syncdbs #}

-- TODO: replace void with error handling
unregisterDB :: Database -> Alpm ()
unregisterDB = void . liftIO . {# call db_unregister #}

-- TODO: db_get_siglevel

-- TODO: replace void with error handling
validDB :: Database -> Alpm ()
validDB = void . liftIO . {# call db_get_valid #}

-- TODO: db_get_servers
-- TODO: db_set_servers

-- TODO: replace void with error handling
addServer :: String -> Database -> Alpm ()
addServer url = void . liftIO . (newCString url >>=) . {# call db_add_server #}

-- TODO: replace void with error handling
removeServer :: String -> Database -> Alpm ()
removeServer url = void . liftIO . (newCString url >>=) . {# call db_remove_server #}

-- TODO: db_update

-- TODO: db_get_group
-- TODO: db_get_groupcache
