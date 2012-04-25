{-# LANGUAGE ForeignFunctionInterface #-}

{# context lib="alpm" prefix="alpm" #}

module Alpm.Unsafe.Database where

import Control.Applicative
import Control.Monad.Trans
import Foreign.C
import Foreign.Ptr

import Alpm.Core
import Alpm.Database
import Alpm.Internal.List
import Alpm.Internal.Types
import Alpm.Utils

{# import Alpm.Internal.Types #}

#include <alpm.h>

package :: String -> Database -> Alpm Package
package pkg = liftIO . (newCString pkg >>=) . {# call db_get_pkg #}

pkgCache :: Database -> Alpm [Package]
pkgCache = liftIO . (toList =<<) . (castPtr <$>) . {# call db_get_pkgcache #}
