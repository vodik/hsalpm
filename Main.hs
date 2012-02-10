module Main where

import Control.Applicative
import Control.Monad.IO.Class

import Alpm
import Alpm.Package

main = do
    pkgs2 <- runAlpm options $ do
        pkgs <- packages <$> localDB
        mapM_ (liftIO . putPkgInfo) pkgs
        return pkgs
    mapM_ putPkgInfo pkgs2
    return ()
  where
    options = defaultOptions

putPkgInfo :: Package -> IO ()
putPkgInfo pkg = do
    putStrLn $ "  PACKAGE: " ++ packageName pkg ++ ": " ++ packageVersion pkg
    putStrLn $ "           " ++ packageDescription pkg
    putStrLn $ "      url: " ++ packageURL pkg
    putStrLn $ "     arch: " ++ packageArch pkg
    putStrLn $ " packager: " ++ packagePackager pkg
    putStrLn ""
