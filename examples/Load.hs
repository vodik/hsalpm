import Control.Monad.Trans
import System.Alpm.Core
import System.Alpm.Transaction
import System.Alpm.Internal.Types
import qualified Data.ByteString.Char8 as BS
import qualified System.Alpm.Unsafe.Database as UD
import qualified System.Alpm.Unsafe.Package as UP

package :: String
package = "/var/cache/pacman/pkg/linux-3.3.4-1-i686.pkg.tar.xz"

loader :: Alpm ()
loader = do
    pkg <- loadPkg package False [ SigUseDefault ]
    liftIO $ do
        BS.putStrLn $ UP.pkgName pkg
        BS.putStrLn $ UP.pkgVersion pkg
        BS.putStrLn $ UP.pkgDescription pkg

main = do
    withAlpm defaultOptions loader >>= either print noop
  where noop = const $ return ()
