module Main where

import Control.Monad

import Alpm
import Alpm.Database
import Alpm.Package
import Pacman

main :: IO ()
main = do
    conf <- getPacman
    runAlpm defaultOptions $ do
        dbs  <- pacmanDBs conf
        forM_ dbs $ updateDB False
