{-# LANGUAGE ForeignFunctionInterface, TypeSynonymInstances, FlexibleInstances #-}

module System.Alpm.Internal.List
    ( AlpmList(..)
    , toList
    ) where

import Control.Applicative
import Control.Monad
import Foreign.C
import Foreign.Ptr
import Foreign.Storable

import System.Alpm.Internal.Types

#include <alpm.h>

data AlpmList a = AlpmList
    { dataPtr :: Ptr a
    , prev    :: Ptr (AlpmList a)
    , next    :: Ptr (AlpmList a)
    }

instance Storable (AlpmList a) where
    sizeOf _    = {# sizeof alpm_list_t #}
    alignment _ = {# alignof alpm_list_t #}

    peek p = AlpmList
        <$> liftM castPtr ({# get alpm_list_t->data #} p)
        <*> liftM castPtr ({# get alpm_list_t->prev #} p)
        <*> liftM castPtr ({# get alpm_list_t->next #} p)
    poke p x = do
        {# set alpm_list_t.data #} p $ castPtr (dataPtr x)
        {# set alpm_list_t.prev #} p $ castPtr (prev x)
        {# set alpm_list_t.next #} p $ castPtr (next x)

toList :: AlpmType a => Ptr (AlpmList a) -> IO [a]
toList ptr
    | isNull ptr = return []
    | otherwise  = peek ptr >>= \(AlpmList d _ n) -> liftM2 (:) (pack d) (toList n)

isNull :: Ptr a -> Bool
isNull = (== nullPtr)
