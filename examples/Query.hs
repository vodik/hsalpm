module Main where

import Control.Applicative
import Control.Monad
import Control.Monad.Reader
import Control.Monad.Trans (liftIO)
import Data.Char
import Data.List
import System.Environment

import Alpm.Core
import Alpm.Database
import Alpm.PkgCache
import Alpm.Internal.Types

queryPkgs :: (Package -> PkgCache Bool) -> Alpm ()
queryPkgs f = syncDBs >>= \dbs -> forM_ dbs $ \db -> do
    withPkgCache db $
        ask >>= filterM f >>= mapM_ (ppPkgInfo $ dbName db)

register :: Alpm ()
register = void $ do
    registerDB "gnome-unstable"    [SigUseDefault]
    registerDB "testing"           [SigUseDefault]
    registerDB "core"              [SigUseDefault]
    registerDB "extra"             [SigUseDefault]
    registerDB "community"         [SigUseDefault]
    registerDB "community-testing" [SigUseDefault]

main :: IO ()
main = do
    args <- getArgs
    rslt <- runAlpm defaultOptions $ do
        register
        queryPkgs $ myFilter args
    case rslt of
        Left  err  -> print err
        Right pkgs -> return ()

myFilter :: [String] -> Package -> PkgCache Bool
myFilter ts pkg = do
    name   <- pkgName pkg
    desc   <- map toLower <$> pkgDescription pkg
    groups <- pkgGroups pkg

    let ts' = map (map toLower) ts
        f1  = all (`isInfixOf` name) ts'
        f2  = all (`isInfixOf` desc) ts'
        f3  = any (`elem` groups) ts'
    return $ f1 || f2 || f3

ppPkgInfo :: String -> Package -> PkgCache ()
ppPkgInfo db pkg = do
    name    <- pkgName pkg
    desc    <- pkgDescription pkg
    version <- pkgVersion pkg
    liftIO . putStrLn $ unwords [ db ++ "/" ++ name, version ]
    liftIO . putStrLn $ "    " ++ desc
