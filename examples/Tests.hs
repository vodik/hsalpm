import Alpm.Core
import Alpm.Database
import Alpm.PkgCache
import Alpm.Transaction
import Alpm.Internal.Types
import Alpm.Options
import Control.Applicative
import Control.Monad.Reader
import Data.List
import qualified Alpm.Unsafe.Database as UD
import qualified Alpm.Unsafe.Package as UP
import qualified Data.ByteString.Char8 as BS

test1 = runAlpm defaultOptions $ do
    dbName <$> localDB

test2 = runAlpm defaultOptions $ do
    map dbName <$> syncDBs

test3 = runAlpm defaultOptions $ do
    registerDB "core" [SigUseDefault]
    map dbName <$> syncDBs

-- Unsafe package cache manipulation
-- test4 :: IO (Either AlpmException BS.ByteString)
test4 = runAlpm defaultOptions $ do
    db <- registerDB "core" [SigUseDefault]
    UP.pkgName <$> UD.package "linux" db

-- Using the PkgCache monad
test5 :: IO (Either AlpmException [String])
test5 = runAlpm defaultOptions $ do
    db <- registerDB "core" [SigUseDefault]
    withPkgCache db $ ask >>= \lst ->
        sort <$> mapM pkgName lst

-- Start an update/transaction
test6 repo = runAlpm defaultOptions $ do
    set [ systemArch ]

    db   <- registerDB repo [SigUseDefault]
    arch <- get arch
    addServer ("http://mirrors.kernel.org/archlinux/" ++ repo ++ "/os/" ++ arch) db

    withTransaction [TransFlagDbonly] $
        updateDB True db

main = do
    test1 >>= either print print
    test2 >>= either print print
    test3 >>= either print print
    test4 >>= either print BS.putStrLn
    test5 >>= either print print
    test6 "extra" >>= either print print
