{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad
import Control.Monad.Trans
import System.Alpm.Core
import System.Alpm.Cache
import System.Alpm.Transaction
import System.Alpm.Options
import System.Alpm.Internal.Types
import System.Environment
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified System.Alpm.Unsafe.Database as UD
import qualified System.Alpm.Unsafe.Package as UP

root   = "/"
dbPath = "/var/lib/pacman/"

loader :: [String] -> Alpm Bool
loader []    = return False
loader paths = do
    set [ useSyslog := True ]

    pkgs <- forM paths $ \path -> do
        pkg <- loadPkg path False [ SigUseDefault ]
        liftIO $ do
            T.putStrLn $ T.unwords [ UP.pkgName pkg, UP.pkgVersion pkg ]
            T.putStrLn $ T.concat  [ "    ", UP.pkgDescription pkg ]
        return pkg

    withTransaction [] $ stageEach Add pkgs
    return True

main = getArgs >>= withAlpm root dbPath . loader >>= either print print
