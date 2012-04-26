{-# LANGUAGE ForeignFunctionInterface #-}

module Alpm.Internal.Callbacks where

import Foreign.C
import Foreign.Ptr

#include <alpm.h>

type LogFunc = CInt -> CString -> IO ()

foreign import ccall "wrapper"
    mkLogFunc :: LogFunc -> IO (FunPtr LogFunc)

type DownloadFunc = CString -> CLong -> CLong -> IO ()

foreign import ccall "wrapper"
    mkDownloadFunc :: DownloadFunc -> IO (FunPtr DownloadFunc)

type FetchFunc = CString -> CString -> CInt -> IO CInt

foreign import ccall "wrapper"
    mkFetchFunc    :: FetchFunc -> IO (FunPtr FetchFunc)

type TotalFunc = CLong -> IO ()

foreign import ccall "wrapper"
    mkTotalFunc    :: TotalFunc -> IO (FunPtr TotalFunc)

type EventFunc a = CInt -> Ptr a -> Ptr a -> IO ()

foreign import ccall "wrapper"
    mkEventFunc    :: EventFunc a -> IO (FunPtr (EventFunc a))

type QuestionFunc a = CInt -> Ptr a -> Ptr a -> Ptr a -> Ptr CInt -> IO ()

foreign import ccall "wrapper"
    mkQuestionFunc :: QuestionFunc a -> IO (FunPtr (QuestionFunc a))

type ProgressFunc = CInt -> CString -> CInt -> CSize -> CSize -> IO ()

foreign import ccall "wrapper"
    mkProgressFunc :: ProgressFunc -> IO (FunPtr ProgressFunc)
