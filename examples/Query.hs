module Main where

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.Trans (liftIO)
import System.Environment
import qualified Data.Text as T

import Alpm.Core
import Alpm.Database
import Alpm.Options
import Alpm.PkgCache
import Alpm.Internal.Types

dbs = [ "testing", "core", "extra", "community", "community-testing" ]

queryPkgs :: (Package -> PkgCache Bool) -> Alpm ()
queryPkgs f = syncDBs >>= \dbs ->
    forM_ dbs $ \db -> withPkgCache db $
        ask >>= filterM f >>= mapM_ (ppPkgInfo $ dbName db)

register :: [String] -> Alpm ()
register = mapM_ $ flip registerDB [ SigUseDefault ]

main :: IO ()
main = do
    args <- map T.pack <$> getArgs
    rslt <- runAlpm defaultOptions $ do
        set [ useSyslog := True ]
        register dbs
        queryPkgs $ myFilter args
    whenLeft print rslt
  where
    whenLeft f = either f . const $ return ()

myFilter :: [T.Text] -> Package -> PkgCache Bool
myFilter ts pkg = do
    name   <- pkgName pkg
    desc   <- T.toLower <$> pkgDescription pkg
    groups <- pkgGroups pkg

    let ts' = map T.toLower ts             -- all keywords compared in lower case
        f1  = all (`T.isInfixOf` name) ts' -- all the keywords are in the name
        f2  = all (`T.isInfixOf` desc) ts' -- all the keywords are in the description
        f3  = any (`elem` groups) ts'      -- any of the keywords is a group
    return $ f1 || f2 || f3

ppPkgInfo :: String -> Package -> PkgCache ()
ppPkgInfo db pkg = do
    name    <- pkgName pkg
    desc    <- pkgDescription pkg
    version <- pkgVersion pkg
    liftIO $ do
        putStrLn $ unwords [ db ++ "/" ++ name, version ]
        putStrLn $ "    " ++ desc
