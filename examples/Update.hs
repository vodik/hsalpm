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
        setProgressCB $ \t str _ _ _ -> do
            putStrLn $ "T: " ++ show t
            peekCString str >>= putStrLn . ("STR: " ++)

        pacmanDBs conf >>= mapM_ (updateDB False)
