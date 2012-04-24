import Alpm.Core
import Alpm.Database
import Alpm.PkgCache
import Alpm.Internal.Types
import Alpm.Options
import Control.Monad.Reader
import qualified Alpm.Unsafe.Database as UD
import qualified Alpm.Unsafe.Package as UP

test1 = runAlpm defaultOptions $ do
    localDB >>= dbName

test2 = runAlpm defaultOptions $ do
    syncDBs >>= mapM dbName

test3 = runAlpm defaultOptions $ do
    registerDB "core" [SigUseDefault]
    syncDBs >>= mapM dbName

-- Unsafe package cache manipulation
test4 = runAlpm defaultOptions $ do
    db <- registerDB "core" [SigUseDefault]
    UD.package "linux" db >>= UP.pkgName

-- Using the PkgCache monad
test5 = runAlpm defaultOptions $ do
    db <- registerDB "core" [SigUseDefault]
    withPkgCache db $
        ask >>= mapM pkgName

-- Playing with options
test6 = runAlpm defaultOptions $ do
    set [ systemArch ]
    get arch

main = do
    test1 >>= either print print
    test2 >>= either print print
    test3 >>= either print print
    test4 >>= either print print
    test5 >>= either print print
    test6 >>= either print print
