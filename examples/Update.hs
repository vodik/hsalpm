module Main where

import Control.Monad
import Foreign.C.String

import Alpm
import Alpm.Database
import Alpm.Package
import Alpm.Option
import Pacman

main :: IO ()
main = do
    conf <- getPacman
    runAlpm defaultOptions $ do
        setProgressCB $ \_ str _ _ _ -> do
            peekCString str >>= putStrLn

        pacmanDBs conf >>= mapM_ (updateDB False)
