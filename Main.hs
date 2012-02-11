module Main where

import Control.Applicative
import Control.Monad.Trans (liftIO)
import Data.List
import System.Environment

import Alpm
import Alpm.Package

main = do
    args <- getArgs
    pkgs <- runAlpm options $ do
        local <- packages <$> localDB
        return $ filter (myFilter $ head args) local
    mapM_ ppPkgInfo pkgs
  where
    options = defaultOptions

myFilter :: String -> Package -> Bool
myFilter term pkg = term `isInfixOf` packageName pkg
                 || term `isInfixOf` packageDescription pkg
                 || term `elem` packageGroups pkg

ppPkgInfo :: Package -> IO ()
ppPkgInfo pkg = do
    putStrLn $ "local/" ++ packageName pkg ++ " " ++ packageVersion pkg ++ " (" ++ unwords (packageGroups pkg) ++ ")"
    putStrLn $ "    " ++ packageDescription pkg
