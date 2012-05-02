import Control.Monad.Trans
import System.Alpm.Core
import System.Alpm.Cache
import System.Alpm.Transaction
import System.Alpm.Options
import System.Alpm.Internal.Types
import System.Environment
import qualified Data.ByteString.Char8 as BS
import qualified System.Alpm.Unsafe.Database as UD
import qualified System.Alpm.Unsafe.Package as UP

root   = "/"
dbPath = "/var/lib/pacman/"

loader :: [String] -> Alpm Bool
loader []    = return False
loader paths = do
    set [ useSyslog := True ]

    pkg <- loadPkg (head paths) False [ SigUseDefault ]
    liftIO $ do
        BS.putStrLn $ UP.pkgName pkg
        BS.putStrLn $ UP.pkgVersion pkg
        BS.putStrLn $ UP.pkgDescription pkg

    withTransaction [] $ stage Add pkg
    return True

main = getArgs >>= withAlpm root dbPath . loader >>= either print print
