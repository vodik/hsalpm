module Main where

import Control.Applicative
import Control.Monad.Trans (liftIO)
import Data.List
import Data.Char
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
myFilter term pkg =
    let term' = map toLower term in
           term' `isInfixOf` packageName pkg
        || term' `isInfixOf` map toLower (packageDescription pkg)
        || term' `elem` packageGroups pkg

ppPkgInfo :: Package -> IO ()
ppPkgInfo pkg = do
    putStrLn $ "local/" ++ packageName pkg ++ " " ++ packageVersion pkg ++ groups (packageGroups pkg)
    putStrLn $ "    " ++ packageDescription pkg
  where
    groups g | null g    = []
             | otherwise = " (" ++ unwords g ++ ")"
