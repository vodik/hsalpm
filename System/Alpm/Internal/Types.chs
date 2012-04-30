{-# LANGUAGE ForeignFunctionInterface, TypeSynonymInstances, FlexibleInstances #-}

module System.Alpm.Internal.Types
    ( AlpmType(..)

    , Database(..)
    , Delta(..)
    , Depend(..)
    , DepMissing(..)
    , File(..)
    , FileConflict(..)
    , FileList(..)
    , Group(..)
    , Package(..)
    , PGPKey(..)
    , SignatureList(..)
    , SignatureResult(..)
    , Trans(..)

    , Reason(..)
    , Origin(..)
    , DepMod(..)
    , FileConflictType(..)
    , SignatureLevel(..)
    , SignatureStatue(..)
    , SignatureValidity(..)
    , LogLevel(..)
    , Event(..)
    , Question(..)
    , Progress(..)
    , TransactionFlags(..)
    , Capabilities(..)
    ) where

import Control.Applicative
import Foreign.C
import Foreign.Ptr
import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T

import System.Alpm.StringLike

#include <alpm.h>

class AlpmType a where
    unpack :: a -> IO (Ptr a)
    pack   :: Ptr a -> IO a

instance AlpmType String where
    unpack = fmap castPtr . toC
    pack   = fromC . castPtr

instance AlpmType B.ByteString where
    unpack = fmap castPtr . toC
    pack   = fromC . castPtr

instance AlpmType T.Text where
    unpack = fmap castPtr . toC
    pack   = fromC . castPtr

{# pointer *alpm_backup_t       as Backup newtype #}
{# pointer *alpm_db_t           as Database newtype #}
{# pointer *alpm_conflict_t     as Conflict newtype #}
{# pointer *alpm_delta_t        as Delta newtype #}
{# pointer *alpm_depend_t       as Depend newtype #}
{# pointer *alpm_depmissing_t   as DepMissing newtype #}
{# pointer *alpm_file_t         as File newtype #}
{# pointer *alpm_fileconflict_t as FileConflict newtype #}
{# pointer *alpm_filelist_t     as FileList newtype #}
{# pointer *alpm_group_t        as Group newtype #}
{# pointer *alpm_pkg_t          as Package newtype #}
{# pointer *alpm_pgpkey_t       as PGPKey newtype #}
{# pointer *alpm_siglist_t      as SignatureList newtype #}
{# pointer *alpm_sigresult_t    as SignatureResult newtype #}
{# pointer *alpm_trans_t        as Trans newtype #}

instance AlpmType Backup where
    unpack (Backup ptr) = return ptr
    pack = return . Backup

instance AlpmType Database where
    unpack (Database ptr) = return ptr
    pack = return . Database

instance AlpmType Conflict where
    unpack (Conflict ptr) = return ptr
    pack = return . Conflict

instance AlpmType Delta where
    unpack (Delta ptr) = return ptr
    pack = return . Delta

instance AlpmType DepMissing where
    unpack (DepMissing ptr) = return ptr
    pack = return . DepMissing

instance AlpmType File where
    unpack (File ptr) = return ptr
    pack = return . File

instance AlpmType FileConflict where
    unpack (FileConflict ptr) = return ptr
    pack = return . FileConflict

instance AlpmType FileList where
    unpack (FileList ptr) = return ptr
    pack = return . FileList

instance AlpmType Group where
    unpack (Group ptr) = return ptr
    pack = return . Group

instance AlpmType Package where
    unpack (Package ptr) = return ptr
    pack = return . Package

instance AlpmType PGPKey where
    unpack (PGPKey ptr) = return ptr
    pack = return . PGPKey

instance AlpmType SignatureList where
    unpack (SignatureList ptr) = return ptr
    pack = return . SignatureList

instance AlpmType SignatureResult where
    unpack (SignatureResult ptr) = return ptr
    pack = return . SignatureResult

instance AlpmType Trans where
    unpack (Trans ptr) = return ptr
    pack = return . Trans

-- | Install Reason
{# enum alpm_pkgreason_t as Reason {underscoreToCase}
    with prefix = "ALPM_" deriving (Eq, Read, Show) #}

-- | Origin
{# enum alpm_pkgfrom_t as Origin {underscoreToCase}
    with prefix = "ALPM_" deriving (Eq, Read, Show) #}

-- | DepMod
{# enum alpm_depmod_t as DepMod {underscoreToCase}
    with prefix = "ALPM_" deriving (Eq, Read, Show) #}

-- | FileConflictType
{# enum alpm_fileconflicttype_t as FileConflictType {underscoreToCase}
    with prefix = "ALPM_" deriving (Eq, Read, Show) #}

-- | Signature Level
{# enum alpm_siglevel_t as SignatureLevel {underscoreToCase}
    with prefix = "ALPM_" deriving (Eq, Read, Show) #}

-- | Signature Status
{# enum alpm_sigstatus_t as SignatureStatue {underscoreToCase}
    with prefix = "ALPM_" deriving (Eq, Read, Show) #}

-- | Signtaure Validity
{# enum alpm_sigvalidity_t as SignatureValidity {underscoreToCase}
    with prefix = "ALPM_" deriving (Eq, Read, Show) #}

-- | LogLevel
{# enum alpm_loglevel_t as LogLevel {underscoreToCase}
    with prefix = "ALPM_" deriving (Eq, Read, Show) #}

-- | AlpmEvent
{# enum alpm_event_t as Event {underscoreToCase}
    with prefix = "ALPM_" deriving (Eq, Read, Show) #}

-- | Question
{# enum alpm_question_t as Question {underscoreToCase}
    with prefix = "ALPM_" deriving (Eq, Read, Show) #}

-- | Progress
{# enum alpm_progress_t as Progress {underscoreToCase}
    with prefix = "ALPM_" deriving (Eq, Read, Show) #}

-- | Transaction Flags
{# enum alpm_transflag_t as TransactionFlags {underscoreToCase}
    with prefix = "ALPM_" deriving (Eq, Read, Show) #}

-- | Capabilities
{# enum alpm_caps as Capabilities {underscoreToCase}
    with prefix = "ALPM_" deriving (Eq, Read, Show) #}
