import Control.Monad.Trans
import System.Alpm.Core
import System.Alpm.Cache
import System.Alpm.Transaction
import System.Alpm.Options
import System.Alpm.Internal.Types
import qualified Data.ByteString.Char8 as BS
import qualified System.Alpm.Unsafe.Database as UD
import qualified System.Alpm.Unsafe.Package as UP

root    = "/"
dbPath  = "/var/lib/pacman/"
package = "/var/cache/pacman/pkg/linux-3.3.4-1-i686.pkg.tar.xz"

loader :: Alpm Bool
loader = do
    set [ useSyslog := True ]

    pkg <- loadPkg package False [ SigUseDefault ]
    liftIO $ do
        BS.putStrLn $ UP.pkgName pkg
        BS.putStrLn $ UP.pkgVersion pkg
        BS.putStrLn $ UP.pkgDescription pkg

    withTransaction [] $ stage Add pkg
    return True

main = withAlpm root dbPath loader >>= either print print
