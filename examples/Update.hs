module Main where

import Control.Monad
import Control.Monad.Trans (liftIO)
import Foreign.C.String

import Alpm
import Alpm.Base
import Alpm.Database
import Alpm.Package
import Alpm.Option
import Alpm.Transaction
import Pacman

main :: IO ()
main = do
    conf <- getPacman
    runAlpm defaultOptions $ do
        setLogCB $ \lvl str -> putStr $ "Logged [" ++ show lvl ++ "]: " ++ str

        core <- registerDB "core"
        addServer core "http://mirrors.kernel.org/archlinux/core/os/i686"
        withTransaction $ updateDB False core
        return ()
