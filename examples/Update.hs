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
        setLogCB $ \_ str -> do
            msg <- peekCString str
            putStr $ "Logged: " ++ msg

        core <- registerDB "core"
        addServer core "http://mirrors.kernel.org/archlinux/core/os/i686"
        withTransaction $ do
            updateDB False core
        return ()
