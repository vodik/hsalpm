module Main where

import Control.Applicative
import Data.List

import Alpm
import Alpm.Package

main = do
    pkgs <- runAlpm options $ do
        local <- flip packagesSorted bySize <$> localDB
        return $ filter (("xorg" `isPrefixOf`) . packageName) local
    mapM_ putPkgInfo pkgs
  where
    options = defaultOptions

putPkgInfo :: Package -> IO ()
putPkgInfo pkg = do
    putStrLn $ "  PACKAGE: " ++ packageName pkg ++ ": " ++ packageVersion pkg
    putStrLn $ "           " ++ packageDescription pkg
    putStrLn $ "     size: " ++ show (packageInstallSize pkg `div` 1024)
    putStrLn $ "      url: " ++ packageURL pkg
    putStrLn $ "     arch: " ++ packageArch pkg
    putStrLn $ " packager: " ++ packagePackager pkg
    putStrLn ""
