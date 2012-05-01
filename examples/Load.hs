import Alpm.Core
import Alpm.Transaction
import Alpm.Internal.Types
import Control.Monad.Trans
import qualified Alpm.Unsafe.Database as UD
import qualified Alpm.Unsafe.Package as UP
import qualified Data.ByteString.Char8 as BS

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
    runAlpm defaultOptions loader >>= either print noop
  where noop = const $ return ()
