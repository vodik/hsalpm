module Main where

import Control.Monad
import Control.Monad.IO.Class

import Alpm
import Alpm.Package

main = withAlpm options $ do
    liftIO $ putStrLn "Hello World!"
    db <- localDB
    pl <- packages db
    mapM_ (liftIO . putPkgInfo) pl
    liftIO $ putStrLn "Done!"
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
