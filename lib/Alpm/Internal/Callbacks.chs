{-# LANGUAGE ForeignFunctionInterface #-}

module Alpm.Internal.Callbacks where

import Foreign.C
import Foreign.Ptr

#include <alpm.h>

foreign import ccall "wrapper" mkLogFunc      :: LogFunc -> IO (FunPtr LogFunc)
foreign import ccall "wrapper" mkDownloadFunc :: DownloadFunc -> IO (FunPtr DownloadFunc)
foreign import ccall "wrapper" mkFetchFunc    :: FetchFunc -> IO (FunPtr FetchFunc)
foreign import ccall "wrapper" mkTotalFunc    :: TotalFunc -> IO (FunPtr TotalFunc)
foreign import ccall "wrapper" mkEventFunc    :: EventFunc a -> IO (FunPtr (EventFunc a))
foreign import ccall "wrapper" mkQuestionFunc :: QuestionFunc a -> IO (FunPtr (QuestionFunc a))
foreign import ccall "wrapper" mkProgressFunc :: ProgressFunc -> IO (FunPtr ProgressFunc)

type LogFunc        = CInt -> CString -> IO ()
type DownloadFunc   = CString -> CLong -> CLong -> IO ()
type FetchFunc      = CString -> CString -> CInt -> IO CInt
type TotalFunc      = CLong -> IO ()
type EventFunc a    = CInt -> Ptr a -> Ptr a -> IO ()
type QuestionFunc a = CInt -> Ptr a -> Ptr a -> Ptr a -> Ptr CInt -> IO ()
type ProgressFunc   = CInt -> CString -> CInt -> CSize -> CSize -> IO ()

logFunc :: (Int -> String -> IO ()) -> IO (FunPtr LogFunc)
logFunc f = mkLogFunc $ \lvl str ->
    peekCString str >>= f (fromIntegral lvl) . reverse . drop 1 . reverse

downloadFunc :: (String -> Int -> Int -> IO ()) -> IO (FunPtr DownloadFunc)
downloadFunc f = mkDownloadFunc $ \name xfer total -> do
    let xfer'  = fromIntegral xfer
        total' = fromIntegral total
    name' <- peekCString name
    f name' xfer' total'
