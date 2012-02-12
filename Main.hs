module Main where

import Control.Applicative
import Control.Monad.Trans (liftIO)
import Data.Char
import Data.Foldable (forM_)
import Data.List
import System.Environment

import Alpm
import Alpm.Database
import Alpm.Package
import Alpm.Network
import Pacman

main = do
    args <- getArgs
    pkgs <- runAlpm options $ concatMap (find args) <$> pacmanDBs
    mapM_ ppPkgInfo pkgs
  where
    find args = filter (myFilter args) . packages
    options   = defaultOptions

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
