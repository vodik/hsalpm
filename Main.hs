module Main where

import Control.Applicative
import Control.Monad.Trans (liftIO)
import Data.Char
import Data.List
import System.Environment
import qualified Data.Foldable as F

import Alpm
import Alpm.Database
import Alpm.Package
import Alpm.Network

main = do
    args <- getArgs
    pkgs <- runAlpm options $ do
        local <- packages <$> localDB
        return $ filter (myFilter args) local
    F.forM_ pkgs ppPkgInfo
  where
    options = defaultOptions

myFilter :: [String] -> Package -> Bool
myFilter ts pkg =
    let ts' = map (map toLower) ts
        f1  = F.all (`isInfixOf` packageName pkg) ts'
        f2  = F.all (`isInfixOf` map toLower (packageDescription pkg)) ts'
        f3  = F.any (`F.elem` packageGroups pkg) ts'
    in f1 || f2 || f3

ppPkgInfo :: Package -> IO ()
ppPkgInfo pkg = do
    putStrLn $ "local/" ++ packageName pkg ++ " " ++ packageVersion pkg ++ groups (packageGroups pkg)
    putStrLn $ "    " ++ packageDescription pkg
  where
    groups g | null g    = []
             | otherwise = " (" ++ unwords g ++ ")"
