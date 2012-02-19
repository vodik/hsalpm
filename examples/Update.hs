module Main where

import Control.Monad
import Control.Monad.Trans (liftIO)
import System.Environment
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
    [arg] <- getArgs
    conf  <- getPacman
    runAlpm defaultOptions $ do
        setLogCB $ \lvl str -> putStr $ "Logged [" ++ show lvl ++ "]: " ++ str

        set [ arch      := "x86_64"
            , logFile   := "/tmp/hsalpm.log"
            , cachePath := [ "/foo" ]
            ]

        cachePath `add`    "/tmp/"
        cachePath `add`    "/yes/"
        cachePath `remove` "/tmp/"

        get arch    >>= liftIO . putStrLn . ("Arch set to: " ++)
        get logFile >>= liftIO . putStrLn . ("Logfile set to: " ++)

        cp <- get cachePath
        mapM_ (liftIO . putStrLn) cp

        core <- registerDB arg
        addServer core $ "http://mirrors.kernel.org/archlinux/" ++ arg ++ "/os/x86_64"

        withTransaction $ do
            updateDB True core
        return ()
