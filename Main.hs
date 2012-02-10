module Main where

import Control.Monad.IO.Class

import Alpm
import Alpm.Package

main = withAlpm options $ localDB >>= mapM_ (liftIO . putPkgInfo) . packages
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
