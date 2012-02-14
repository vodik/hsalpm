module Main where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans (liftIO)
import Data.Char
import Data.List
import System.Environment

import Alpm
import Alpm.Database
import Alpm.Package
import Pacman

main :: IO ()
main = do
    args <- getArgs
    conf <- getPacman
    pkgs <- runAlpm defaultOptions $ do
        dbs  <- pacmanDBs conf
        pkgs <- forM dbs $ getFromPkgCache $ return . filter (myFilter args)
        return $ concat pkgs
    mapM_ ppPkgInfo pkgs

myFilter :: [String] -> Package -> Bool
myFilter ts pkg =
    let ts' = map (map toLower) ts
        f1  = all (`isInfixOf` packageName pkg) ts'
        f2  = all (`isInfixOf` map toLower (packageDescription pkg)) ts'
        f3  = any (`elem` packageGroups pkg) ts'
    in f1 || f2 || f3

ppPkgName :: Package -> IO ()
ppPkgName pkg = putStrLn $ unwords [ packageName pkg, packageVersion pkg ]

ppPkgInfo :: Package -> IO ()
ppPkgInfo pkg = do
    putStrLn $ packageDB pkg ++ "/" ++ packageName pkg ++ " " ++ packageVersion pkg ++ groups (packageGroups pkg)
    putStrLn $ "    " ++ packageDescription pkg
  where
    groups g | null g    = []
             | otherwise = " (" ++ unwords g ++ ")"
