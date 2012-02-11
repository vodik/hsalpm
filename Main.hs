module Main where

import Control.Applicative
import Control.Monad.Trans (liftIO)
import Control.Parallel (par)
import Data.List
import Data.Char
import System.Environment

import Alpm
import Alpm.Package

main = do
    args <- getArgs
    pkgs <- runAlpm options $ do
        local <- packages <$> localDB
        return $ filter (myFilter args) local
    mapM_ ppPkgInfo pkgs
  where
    options = defaultOptions

myFilter :: [String] -> Package -> Bool
myFilter ts pkg =
    let ts' = map (map toLower) ts
        f1  = all (`isInfixOf` packageName pkg) ts'
        f2  = all (`isInfixOf` map toLower (packageDescription pkg)) ts'
        f3  = any (`elem` packageGroups pkg) ts'
    in f1 `par` f2 `par` f3 `par` f1 || f2 || f3

ppPkgInfo :: Package -> IO ()
ppPkgInfo pkg = do
    putStrLn $ "local/" ++ packageName pkg ++ " " ++ packageVersion pkg ++ groups (packageGroups pkg)
    putStrLn $ "    " ++ packageDescription pkg
  where
    groups g | null g    = []
             | otherwise = " (" ++ unwords g ++ ")"
