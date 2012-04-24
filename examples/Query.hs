module Main where

import Control.Applicative
import Control.Monad
import Control.Monad.Reader
import Control.Monad.Trans (liftIO)
import Data.Char
import Data.List
import System.Environment

import Alpm.Core
import Alpm.Internal.Types
import Alpm.Unsafe.Package (Package)
import Alpm.Database
import Alpm.PkgCache

queryPkgs :: (Package -> PkgCache Bool) -> Alpm ()
queryPkgs f = do
    dbs <- syncDBs
    withPkgCaches dbs $ do
        pkgs <- ask >>= filterM f
        mapM_ ppPkgInfo pkgs

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
    rslt <- runAlpm defaultOptions $
        register >> queryPkgs (myFilter args)
    case rslt of
        Left  err  -> print err
        Right pkgs -> return ()

myFilter :: [String] -> Package -> PkgCache Bool
myFilter ts pkg = do
    name <- pkgName pkg
    desc <- pkgDescription pkg
    let ts' = map (map toLower) ts
        f1  = all (`isInfixOf` name) ts'
        f2  = all (`isInfixOf` map toLower desc) ts'
        -- f3  = any (`elem` pkgGroups pkg) ts'
    return $ f1 || f2 -- || f3

ppPkgInfo :: Package -> PkgCache ()
ppPkgInfo pkg = do
    name    <- pkgName pkg
    desc    <- pkgDescription pkg
    version <- pkgVersion pkg
    liftIO . putStrLn $ unwords [ "local" ++ "/" ++ name, version ]
    liftIO . putStrLn $ "    " ++ desc
