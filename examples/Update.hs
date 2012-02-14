module Main where

import Control.Monad
import Control.Monad.Trans (liftIO)
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

        addServer "http://mirrors.kernel.org/archlinux/core/os/i686"
        -- mapM_ (liftIO . putStrLn) =<< servers

        -- pacmanDBs conf >>= mapM_ (updateDB False)
