module Main where

import Control.Monad
import Control.Monad.Trans (liftIO)
import System.Environment
import System.IO

import Alpm
import Alpm.Base
import Alpm.Database
import Alpm.Package
import Alpm.Option
import Alpm.Transaction
import Pacman

logMsg :: Int -> String -> IO ()
logMsg lvl str = hPutStrLn stderr $ concat [ "Logged [", show lvl, "]: ", str ]

printErr :: AlpmException -> IO ()
printErr (AlpmException m a) = hPutStrLn stderr $ concat [ "ERROR: ", m, ": ", a ]
printErr UnknownException    = hPutStrLn stderr "UNKNOWN ERROR!"

update :: String -> Alpm ()
update repo = do
    onLog logMsg

    set [ systemArch
        , logFile    := "/tmp/hsalpm.log"
        , cachePath  := [ "/tmp/" ]
        , useSyslog  := True ]

    db   <- registerDB repo
    arch <- get arch
    addServer db $ "isdf://mirrors.kernel.org/archlinux/" ++ repo ++ "/os/" ++ arch

    withTransaction $
        updateDB True db
    return ()

main :: IO ()
main = do
    [arg]  <- getArgs
    result <- runAlpm defaultOptions $ update arg
    case result of
        Left err -> printErr err
        Right _  -> return ()
