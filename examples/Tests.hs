import Control.Applicative
import Control.Monad.Reader
import Data.List
import System.Alpm.Core
import System.Alpm.Cache
import System.Alpm.Database
import System.Alpm.PkgCache
import System.Alpm.Transaction
import System.Alpm.Internal.Alpm
import System.Alpm.Internal.Types
import System.Alpm.Options
import qualified Data.ByteString.Char8 as BS
import qualified System.Alpm.Unsafe.Database as UD
import qualified System.Alpm.Unsafe.Package as UP

root   = "/"
dbPath = "/var/lib/pacman/"
mirror = "http://mirrors.kernel.org/archlinux/"

test1 = withAlpm root dbPath $ dbName <$> localDB

test2 = withAlpm root dbPath $ do
    registerDB "core" [ SigUseDefault ]
    map dbName <$> syncDBs

-- Unsafe package cache manipulation
test3 = withAlpm root dbPath $ do
    db <- registerDB "core" [ SigUseDefault ]
    UP.pkgName <$> UD.package "linux" db

-- Using the PkgCache monad
test4 = withAlpm root dbPath $ do
    db <- registerDB "core" [ SigUseDefault ]
    withPkgCache db $ sort <$> (cache >>= mapM pkgName)

-- Start an update/transaction
test5 repo = withAlpm root dbPath $ do
    set [ systemArch, useSyslog := True ]

    db   <- registerDB repo [SigUseDefault]
    arch <- get arch
    addServer (mirror ++ repo ++ "/os/" ++ arch) db

    withTransaction [TransFlagDbonly] $
        updateDB True db

main = do
    alpmVersion >>= BS.putStrLn
    alpmCapabilities >>= print

    test1 >>= either print BS.putStrLn
    test2 >>= either print (mapM_ BS.putStrLn)
    test3 >>= either print BS.putStrLn
    test4 >>= either print (mapM_ BS.putStrLn)
    test5 "extra" >>= either print print
