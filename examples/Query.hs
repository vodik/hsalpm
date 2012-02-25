module Main where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans (liftIO)
import Data.Char
import Data.List
import System.Environment

import Alpm
import Alpm.Base
import Alpm.Database
import Alpm.Package
import Pacman

queryPkgs :: t -> (Package -> Bool) -> Alpm [Package]
queryPkgs conf f = do
    dbs <- pacmanDBs conf
    withPkgCaches dbs $ filter f

main :: IO ()
main = do
    args <- getArgs
    conf <- getPacman
    rslt <- runAlpm defaultOptions $ queryPkgs conf (myFilter args)
    case rslt of
        Left  err  -> print err
        Right pkgs -> mapM_ ppPkgInfo pkgs

myFilter :: [String] -> Package -> Bool
myFilter ts pkg =
    let ts' = map (map toLower) ts
        f1  = all (`isInfixOf` packageName pkg) ts'
        f2  = all (`isInfixOf` map toLower (packageDescription pkg)) ts'
        f3  = any (`elem` packageGroups pkg) ts'
    in f1 || f2 || f3

ppPkgInfo :: Package -> IO ()
ppPkgInfo pkg = do
    putStrLn $ unwords [ packageDB pkg ++ "/" ++ packageName pkg, packageVersion pkg ]
    putStrLn $ "    " ++ packageDescription pkg
