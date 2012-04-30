{-# LANGUAGE ForeignFunctionInterface #-}

{# context lib="alpm" prefix="alpm" #}

module System.Alpm.Unsafe.Database where

import Control.Applicative
import Control.Monad.Trans
import Foreign.C
import Foreign.Ptr

import System.Alpm.Core
import System.Alpm.Database
import System.Alpm.Internal.List
import System.Alpm.Internal.Types
import System.Alpm.StringLike
import System.Alpm.Utils

{# import System.Alpm.Internal.Types #}

#include <alpm.h>

package :: StringLike a => a -> Database -> Alpm Package
package pkg = liftIO . (toC pkg >>=) . {# call db_get_pkg #}

pkgCache :: Database -> Alpm [Package]
pkgCache = liftIO . (toList =<<) . (castPtr <$>) . {# call db_get_pkgcache #}
