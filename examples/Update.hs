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

update :: String -> Alpm ()
update repo = do
    -- setLogCB $ \lvl str ->
    --     putStr $ "Logged [" ++ show lvl ++ "]: " ++ str

    set [ systemArch
        , logFile    := "/tmp/hsalpm.log"
        , cachePath  := [ "/tmp/" ]
        , useSyslog  := True
        ]

    get arch       >>= liftIO . putStrLn . ("Arch:    " ++)
    get logFile    >>= liftIO . putStrLn . ("Logfile: " ++)
    -- get gpgDirectory >>= liftIO . putStrLn . ("GPG: " ++)
    get cachePath  >>= liftIO . putStrLn . ("Cache:   " ++) . unwords
    get ignorePkgs >>= liftIO . putStrLn . ("Ignore:  " ++) . unwords
    get useSyslog  >>= liftIO . putStrLn . ("Syslog:  " ++) . show
    get useDelta   >>= liftIO . putStrLn . ("Deltas:  " ++) . show
    get checkSpace >>= liftIO . putStrLn . ("Space:   " ++) . show

    core <- registerDB repo
    get arch >>= \a -> addServer core $ "http://mirrors.kernel.org/archlinux/" ++ repo ++ "/os/" ++ a

    withTransaction $
        updateDB True core
    return ()

main :: IO ()
main = do
    [arg]  <- getArgs
    result <- runAlpm defaultOptions $ update arg
    case result of
        Left err -> print err
        Right _  -> return ()
