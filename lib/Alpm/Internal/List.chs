{-# LANGUAGE ForeignFunctionInterface, TypeSynonymInstances, FlexibleInstances #-}

module Alpm.Internal.List
    ( AlpmList(..)
    ) where

import Control.Applicative
import Foreign.C
import Foreign.Ptr
import Foreign.Storable

#include <alpm.h>

data AlpmList = AlpmList
    { dataPtr :: Ptr ()
    , prev    :: Ptr ()
    , next    :: Ptr ()
    }

instance Storable AlpmList where
    sizeOf _    = {# sizeof alpm_list_t #}
    alignment _ = {# alignof alpm_list_t #}

    peek p = AlpmList
        <$> {# get alpm_list_t->data #} p
        <*> {# get alpm_list_t->prev #} p
        <*> {# get alpm_list_t->next #} p
    poke p x = do
        {# set alpm_list_t.data #} p $ dataPtr x
        {# set alpm_list_t.prev #} p $ prev x
        {# set alpm_list_t.next #} p $ next x
