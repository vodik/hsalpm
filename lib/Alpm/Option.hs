{-# LANGUAGE ExistentialQuantification, ForeignFunctionInterface #-}

module Alpm.Option where

import Control.Applicative
import Control.DeepSeq
import Control.Monad.Reader
import Data.Word
import System.IO.Unsafe
import System.Posix.Unistd
import Text.Printf

import Foreign.C
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils
import Foreign.Storable

import Alpm.Base
import Alpm.Database
import Alpm.List
import Alpm.Network
import Alpm.Package
import Alpm.Util

data Attr a b = Attr     (Alpm a) (a -> Alpm ())
              | ListAttr (Alpm a) (a -> Alpm ()) (b -> Alpm ()) (b -> Alpm ())

data AttrOp = forall a b. Attr a b :=  a
            | forall a b. Attr a b :~  (a -> a)
            | forall a b. Attr a b :=> (Alpm a)
            | forall a b. Attr a b :~> (a -> Alpm a)

infixr 0 :=, :~, :=>, :~>

get :: Attr a b -> Alpm a
get (Attr     get _    ) = get
get (ListAttr get _ _ _) = get

set :: [AttrOp] -> Alpm ()
set = mapM_ app
  where
    app (Attr     get set     :=  x) = set x
    app (Attr     get set     :~  f) = get >>= set . f
    app (Attr     get set     :=> x) =   x >>= set
    app (Attr     get set     :~> f) = get >>= f >>= set
    app (ListAttr get set _ _ :=  x) = set x
    app (ListAttr get set _ _ :~  f) = get >>= set . f
    app (ListAttr get set _ _ :=> x) =   x >>= set
    app (ListAttr get set _ _ :~> f) = get >>= f >>= set

add :: Attr a b -> b -> Alpm ()
add (ListAttr _ _ add _) x = add x
add _                    _ = return ()

remove :: Attr a b -> b -> Alpm ()
remove (ListAttr _ _ _ remove) x = remove x
remove _                       _ = return ()

--------------------------------------------------------------------------------

newAlpmStringAttr get set = Attr alpmGetter alpmSetter
  where
    alpmGetter   = withAlpmPtr $ \ptr -> get ptr >>= nullStr ""
    alpmSetter v = withAlpmPtr $ \ptr -> newCString v >>= set ptr
    nullStr s r  = if isNull r then return "" else peekCString r

foreign import ccall "alpm_option_get_arch" c_alpm_option_get_arch :: Ptr AlpmHandle -> IO CString
foreign import ccall "alpm_option_set_arch" c_alpm_option_set_arch :: Ptr AlpmHandle -> CString -> IO ()
arch = newAlpmStringAttr c_alpm_option_get_arch
                         c_alpm_option_set_arch

foreign import ccall "alpm_option_get_logfile" c_alpm_option_get_logfile :: Ptr AlpmHandle -> IO CString
foreign import ccall "alpm_option_set_logfile" c_alpm_option_set_logfile :: Ptr AlpmHandle -> CString -> IO ()
logFile = newAlpmStringAttr c_alpm_option_get_logfile
                            c_alpm_option_set_logfile

foreign import ccall "alpm_option_get_gpgdir" c_alpm_option_get_gpgdir :: Ptr AlpmHandle -> IO CString
foreign import ccall "alpm_option_set_gpgdir" c_alpm_option_set_gpgdir :: Ptr AlpmHandle -> CString -> IO ()
gpgDirectory = newAlpmStringAttr c_alpm_option_get_gpgdir
                                 c_alpm_option_set_gpgdir

--------------------------------------------------------------------------------

newAlpmToggleAttr get set = Attr alpmGetter alpmSetter
  where
    alpmGetter   = toBool <$> withAlpmPtr get
    alpmSetter v = withAlpmPtr $ flip set $ fromBool v

foreign import ccall "alpm_option_get_usesyslog" c_alpm_option_get_usesyslog :: Ptr AlpmHandle -> IO CInt
foreign import ccall "alpm_option_set_usesyslog" c_alpm_option_set_usesyslog :: Ptr AlpmHandle -> CInt -> IO ()
useSyslog = newAlpmToggleAttr c_alpm_option_get_usesyslog
                              c_alpm_option_set_usesyslog

foreign import ccall "alpm_option_get_usedelta" c_alpm_option_get_usedelta :: Ptr AlpmHandle -> IO CInt
foreign import ccall "alpm_option_set_usedelta" c_alpm_option_set_usedelta :: Ptr AlpmHandle -> CInt -> IO ()
useDelta = newAlpmToggleAttr c_alpm_option_get_usedelta
                             c_alpm_option_set_usedelta

foreign import ccall "alpm_option_get_checkspace" c_alpm_option_get_checkspace :: Ptr AlpmHandle -> IO CInt
foreign import ccall "alpm_option_set_checkspace" c_alpm_option_set_checkspace :: Ptr AlpmHandle -> CInt -> IO ()
checkSpace = newAlpmToggleAttr c_alpm_option_get_checkspace
                               c_alpm_option_set_checkspace

--------------------------------------------------------------------------------

newAlpmListAttr get set add remove = ListAttr alpmGetter alpmSetter alpmAdder alpmRemover
  where
    alpmGetter    = withAlpmPtr $ \ptr -> unpackAlpmList <$> get ptr :: IO [String]
    alpmSetter  v = withAlpmPtr $ \ptr -> withForeignPtr (packAlpmList v) $ set ptr
    alpmAdder   v = withAlpmPtr $ \ptr -> newCString v >>= add ptr
    alpmRemover v = withAlpmPtr $ \ptr -> newCString v >>= remove ptr

foreign import ccall "alpm_option_get_cachedirs"   c_alpm_option_get_cachedirs   :: Ptr AlpmHandle -> IO (Ptr (AlpmList CChar))
foreign import ccall "alpm_option_set_cachedirs"   c_alpm_option_set_cachedirs   :: Ptr AlpmHandle -> Ptr (AlpmList CChar) -> IO ()
foreign import ccall "alpm_option_add_cachedir"    c_alpm_option_add_cachedir    :: Ptr AlpmHandle -> CString -> IO ()
foreign import ccall "alpm_option_remove_cachedir" c_alpm_option_remove_cachedir :: Ptr AlpmHandle -> CString -> IO ()
cachePath = newAlpmListAttr c_alpm_option_get_cachedirs
                            c_alpm_option_set_cachedirs
                            c_alpm_option_add_cachedir
                            c_alpm_option_remove_cachedir

foreign import ccall "alpm_option_get_noupgrades"   c_alpm_option_get_noupgrades   :: Ptr AlpmHandle -> IO (Ptr (AlpmList CChar))
foreign import ccall "alpm_option_set_noupgrades"   c_alpm_option_set_noupgrades   :: Ptr AlpmHandle -> Ptr (AlpmList CChar) -> IO ()
foreign import ccall "alpm_option_add_noupgrade"    c_alpm_option_add_noupgrade    :: Ptr AlpmHandle -> CString -> IO ()
foreign import ccall "alpm_option_remove_noupgrade" c_alpm_option_remove_noupgrade :: Ptr AlpmHandle -> CString -> IO ()
noUpgrades = newAlpmListAttr c_alpm_option_get_noupgrades
                             c_alpm_option_set_noupgrades
                             c_alpm_option_add_noupgrade
                             c_alpm_option_remove_noupgrade

foreign import ccall "alpm_option_get_noextracts"   c_alpm_option_get_noextracts   :: Ptr AlpmHandle -> IO (Ptr (AlpmList CChar))
foreign import ccall "alpm_option_set_noextracts"   c_alpm_option_set_noextracts   :: Ptr AlpmHandle -> Ptr (AlpmList CChar) -> IO ()
foreign import ccall "alpm_option_add_noextract"    c_alpm_option_add_noextract    :: Ptr AlpmHandle -> CString -> IO ()
foreign import ccall "alpm_option_remove_noextract" c_alpm_option_remove_noextract :: Ptr AlpmHandle -> CString -> IO ()
noExtracts = newAlpmListAttr c_alpm_option_get_noextracts
                             c_alpm_option_set_noextracts
                             c_alpm_option_add_noextract
                             c_alpm_option_remove_noextract

foreign import ccall "alpm_option_get_ignorepkgs"   c_alpm_option_get_ignorepkgs   :: Ptr AlpmHandle -> IO (Ptr (AlpmList CChar))
foreign import ccall "alpm_option_set_ignorepkgs"   c_alpm_option_set_ignorepkgs   :: Ptr AlpmHandle -> Ptr (AlpmList CChar) -> IO ()
foreign import ccall "alpm_option_add_ignorepkg"    c_alpm_option_add_ignorepkg    :: Ptr AlpmHandle -> CString -> IO ()
foreign import ccall "alpm_option_remove_ignorepkg" c_alpm_option_remove_ignorepkg :: Ptr AlpmHandle -> CString -> IO ()
ignorePkgs = newAlpmListAttr c_alpm_option_get_ignorepkgs
                             c_alpm_option_set_ignorepkgs
                             c_alpm_option_add_ignorepkg
                             c_alpm_option_remove_ignorepkg

foreign import ccall "alpm_option_get_ignoregroups"   c_alpm_option_get_ignoregroups   :: Ptr AlpmHandle -> IO (Ptr (AlpmList CChar))
foreign import ccall "alpm_option_set_ignoregroups"   c_alpm_option_set_ignoregroups   :: Ptr AlpmHandle -> Ptr (AlpmList CChar) -> IO ()
foreign import ccall "alpm_option_add_ignoregroup"    c_alpm_option_add_ignoregroup    :: Ptr AlpmHandle -> CString -> IO ()
foreign import ccall "alpm_option_remove_ignoregroup" c_alpm_option_remove_ignoregroup :: Ptr AlpmHandle -> CString -> IO ()
ignoreGroups = newAlpmListAttr c_alpm_option_get_ignoregroups
                               c_alpm_option_set_ignoregroups
                               c_alpm_option_add_ignoregroup
                               c_alpm_option_remove_ignoregroup

--------------------------------------------------------------------------------

systemArch = arch :=> liftIO $ machine <$> getSystemID

--------------------------------------------------------------------------------

type AlpmLogCB        = CInt -> CString -> IO ()
type AlpmDownloadCB   = CString -> CLong -> CLong -> IO ()
type AlpmFetchCB      = CString -> CString -> CInt -> IO CInt
type AlpmTotalCB      = CLong -> IO ()
type AlpmEventCB a    = CInt -> Ptr a -> Ptr a -> IO ()
type AlpmQuestionCB a = CInt -> Ptr a -> Ptr a -> Ptr a -> Ptr CInt -> IO ()
type AlpmProgressCB   = CInt -> CString -> CInt -> CSize -> CSize -> IO ()

foreign import ccall "wrapper" wrap_cb_log      :: AlpmLogCB -> IO (FunPtr AlpmLogCB)
foreign import ccall "wrapper" wrap_cb_download :: AlpmDownloadCB -> IO (FunPtr AlpmDownloadCB)
foreign import ccall "wrapper" wrap_cb_fetch    :: AlpmFetchCB -> IO (FunPtr AlpmFetchCB)
foreign import ccall "wrapper" wrap_cb_total    :: AlpmTotalCB -> IO (FunPtr AlpmTotalCB)
foreign import ccall "wrapper" wrap_cb_event    :: AlpmEventCB a -> IO (FunPtr (AlpmEventCB a))
foreign import ccall "wrapper" wrap_cb_question :: AlpmQuestionCB a -> IO (FunPtr (AlpmQuestionCB a))
foreign import ccall "wrapper" wrap_cb_progress :: AlpmProgressCB -> IO (FunPtr AlpmProgressCB)

foreign import ccall "alpm_option_set_logcb"
    c_alpm_option_set_logcb :: Ptr AlpmHandle -> FunPtr AlpmLogCB -> IO CInt
foreign import ccall "alpm_option_set_dlcb"
    c_alpm_option_set_dlcb :: Ptr AlpmHandle -> FunPtr AlpmDownloadCB -> IO CInt
foreign import ccall "alpm_option_set_fetchcb"
    c_alpm_option_set_fetchcb :: Ptr AlpmHandle -> FunPtr AlpmFetchCB -> IO CInt
foreign import ccall "alpm_option_set_totalcb"
    c_alpm_option_set_totalcb :: Ptr AlpmHandle -> FunPtr AlpmTotalCB -> IO CInt
foreign import ccall "alpm_option_set_eventcb"
    c_alpm_option_set_eventcb :: Ptr AlpmHandle -> FunPtr (AlpmEventCB a) -> IO CInt
foreign import ccall "alpm_option_set_questioncb"
    c_alpm_option_set_questioncb :: Ptr AlpmHandle -> FunPtr (AlpmQuestionCB a) -> IO CInt
foreign import ccall "alpm_option_set_progresscb"
    c_alpm_option_set_progresscb :: Ptr AlpmHandle -> FunPtr AlpmProgressCB -> IO CInt

data AlpmQuestion = AlpmQuestion String
type AlpmEventCode = Int

data AlpmEvent = CheckdepsStart
               | CheckdepsDone
               | FileconflictsStart
               | FileconflictsDone
               | ResolvedepsStart
               | ResolvedepsDone
               | InterconflictsStart
               | InterconflictsDone
               | AddStart
               | AddDone
               | RemoveStart
               | RemoveDone
               | UpgradeStart
               | UpgradeDone
               | IntegrityStart
               | IntegrityDone
               | LoadStart
               | LoadDone
               | DeltaIntegrityStart
               | DeltaIntegrityDone
               | DeltaPatchesStart
               | DeltaPatchesDone
               | DeltaPatchStart
               | DeltaPatchDone
               | DeltaPatchFailed
               | ScriptletInfo
               | RetrieveStart
               | DiskspaceStart
               | DiskspaceDone
               deriving (Enum, Eq, Read, Show)

data EventType = CheckDepends
               | FileConflicts
               | ResolveDependancies
               | InterConflicts
               | Add Package
               | Remove Package
               | Upgrade Package
               | IntegrityCheck
               | Loading
               | DeltaIntegrityCheck
               | DeltaPatches
               | Retreive
               | Diskspace

data Event = Start EventType
           | Done EventType
           | Failed EventType
           | Info String
           | Unknown

onLog :: (Int -> String -> IO ()) -> Alpm ()
onLog f = do
    cbW <- liftIO . wrap_cb_log $ \lvl str ->
        peekCString str >>= f (fromIntegral lvl)
    withAlpmPtr $ flip c_alpm_option_set_logcb cbW
    return ()

onDownload :: (String -> Int -> Int -> IO ()) -> Alpm ()
onDownload f = do
    cbW <- liftIO . wrap_cb_download $ \a b c -> do
        let b' = fromIntegral b
            c' = fromIntegral c
        a' <- peekCString a
        f a' b' c'
    withAlpmPtr $ flip c_alpm_option_set_dlcb cbW
    return ()

onFetch :: (String -> String -> Bool -> IO Int) -> Alpm ()
onFetch f = do
    cbW <- liftIO . wrap_cb_fetch $ \a b c -> do
        a' <- peekCString a
        b' <- peekCString b
        fromIntegral <$> f a' b' (toBool c)
    withAlpmPtr $ flip c_alpm_option_set_fetchcb cbW
    return ()

onTotal :: (Int -> IO ()) -> Alpm ()
onTotal f = do
    cbW <- liftIO . wrap_cb_total $ \a ->
        f $ fromIntegral a
    withAlpmPtr $ flip c_alpm_option_set_totalcb cbW
    return ()

onEvent :: (Event -> IO ()) -> Alpm ()
onEvent f = do
    cbW <- liftIO . wrap_cb_event $ \event d1 d2 ->
        let pkg = unpack (castPtr d1 :: Ptr PkgHandle)
            msg = case toEnum . subtract 1 $ fromIntegral event of
                CheckdepsStart      -> Start CheckDepends
                CheckdepsDone       -> Done  CheckDepends
                FileconflictsStart  -> Start FileConflicts
                FileconflictsDone   -> Done  FileConflicts
                ResolvedepsStart    -> Start ResolveDependancies
                ResolvedepsDone     -> Done  ResolveDependancies
                InterconflictsStart -> Start InterConflicts
                InterconflictsDone  -> Done  InterConflicts
                AddStart            -> Start $ Add $!! pkg
                AddDone             -> Done  $ Add $!! pkg
                RemoveStart         -> Start $ Remove $!! pkg
                RemoveDone          -> Done  $ Remove $!! pkg
                UpgradeStart        -> Start $ Upgrade $!! pkg
                UpgradeDone         -> Done  $ Upgrade $!! pkg
                IntegrityStart      -> Start IntegrityCheck
                IntegrityDone       -> Done  IntegrityCheck
                LoadStart           -> Start Loading
                LoadDone            -> Done  Loading
                DeltaIntegrityStart -> Start DeltaIntegrityCheck
                DeltaIntegrityDone  -> Done  DeltaIntegrityCheck
                DeltaPatchesStart   -> undefined
                DeltaPatchesDone    -> undefined
                DeltaPatchStart     -> undefined
                DeltaPatchDone      -> undefined
                DeltaPatchFailed    -> undefined
                ScriptletInfo       -> undefined
                RetrieveStart       -> Start Retreive
                DiskspaceStart      -> Start Diskspace
                DiskspaceDone       -> Done  Diskspace
        in f msg
    withAlpmPtr $ flip c_alpm_option_set_eventcb cbW
    return()

onQuestion :: (AlpmQuestion -> IO Bool) -> Alpm ()
onQuestion f = do
    cbW <- liftIO . wrap_cb_question $ \a b c d e -> do
        f $ AlpmQuestion "foo"
        return ()
    withAlpmPtr $ flip c_alpm_option_set_questioncb cbW
    return()

onProgress :: (Int -> String -> Int -> Word -> Word -> IO ()) -> Alpm ()
onProgress f = do
    cbW <- liftIO . wrap_cb_progress $ \a b c d e -> do
        let a' = fromIntegral a
            c' = fromIntegral c
            d' = fromIntegral d
            e' = fromIntegral e
        b' <- peekCString b
        f a' b' c' d' e'
    withAlpmPtr $ flip c_alpm_option_set_progresscb cbW
    return ()
