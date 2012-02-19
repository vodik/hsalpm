module Main where

import Control.Monad
import Control.Monad.Trans (liftIO)
import System.Environment

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
        setLogCB $ \lvl str ->
            putStr $ "Logged [" ++ show lvl ++ "]: " ++ str

        set [ systemArch
            , logFile    := "/tmp/hsalpm.log"
            , cachePath  := [ "/tmp/" ]
            , useSyslog  := True
            ]

        get arch       >>= liftIO . putStrLn . ("Arch:    " ++)
        get logFile    >>= liftIO . putStrLn . ("Logfile: " ++)
        get cachePath  >>= liftIO . putStrLn . ("Cache:   " ++) . unwords
        get ignorePkgs >>= liftIO . putStrLn . ("Ignore:  " ++) . unwords
        get useSyslog  >>= liftIO . putStrLn . ("Syslog:  " ++) . show
        get useDelta   >>= liftIO . putStrLn . ("Deltas:  " ++) . show
        get checkSpace >>= liftIO . putStrLn . ("Space:   " ++) . show

        core <- registerDB arg
        addServer core $ "http://mirrors.kernel.org/archlinux/" ++ arg ++ "/os/x86_64"

        withTransaction $ do
            updateDB True core
        return ()
