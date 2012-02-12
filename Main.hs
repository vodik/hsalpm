module Main where

import Control.Applicative
import Control.Monad.Trans (liftIO)
import Data.Char
import Data.List
import System.Environment

import Alpm
import Alpm.Database
import Alpm.Package
import Alpm.Network
import Pacman

main :: IO ()
main = do
    args <- getArgs
    conf <- getPacman
    pkgs <- runAlpm defaultOptions $ do
        dbs <- pacmanDBs conf
        let func = filter (myFilter args) . packages
        return $ concatMap func dbs
    mapM_ ppPkgInfo pkgs

myFilter :: [String] -> Package -> Bool
myFilter ts pkg =
    let ts' = map (map toLower) ts
        f1  = all (`isInfixOf` packageName pkg) ts'
        f2  = all (`isInfixOf` map toLower (packageDescription pkg)) ts'
        f3  = any (`elem` packageGroups pkg) ts'
    in f1 || f2 || f3

ppPkgInfo :: Package -> IO ()
ppPkgInfo pkg = do
    putStrLn $ packageDB pkg ++ "/" ++ packageName pkg ++ " " ++ packageVersion pkg ++ groups (packageGroups pkg)
    putStrLn $ "    " ++ packageDescription pkg
  where
    groups g | null g    = []
             | otherwise = " (" ++ unwords g ++ ")"
