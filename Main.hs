module Main where

import Control.Monad
import Control.Monad.IO.Class

import Alpm
import Alpm.Package

main = withAlpm options $ do
    liftIO $ putStrLn "Hello World!"
    db <- localDB
    pc <- packageCache db
    pl <- packages pc
    mapM_ (liftIO . putPkgInfo) pl
    liftIO $ putStrLn "Done!"
  where
    options = defaultOptions
    putPkgInfo pkg = do
        putStrLn $ "PACKAGE: " ++ packageName pkg ++ ": " ++ packageVersion pkg
        putStrLn $ "         " ++ packageDescription pkg
        putStrLn $ "   size: " ++ (show $ packageSize pkg)
        putStrLn $ "    url: " ++ packageURL pkg
        putStrLn ""
