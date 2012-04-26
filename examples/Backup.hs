module Main where

import Control.Applicative
import Control.Monad
import Control.Monad.Reader
import Control.Monad.Trans (liftIO)
import qualified Data.Text as T
import qualified Data.Text.IO as TI

import Alpm.Core
import Alpm.Database
import Alpm.PkgCache
import Alpm.Internal.Types
import qualified Alpm.Unsafe.Database as UD
import qualified Alpm.Unsafe.Package as UP

findBackups :: Alpm ()
findBackups = localDB >>= \d ->
    withPkgCache d $ do
        cache <- ask
        field <- concat <$> mapM pkgConflicts cache :: PkgCache [T.Text]
        liftIO $ print field

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
    rslt <- runAlpm defaultOptions $ do
        register
        findBackups
    case rslt of
        Left  err  -> print err
        Right pkgs -> return ()
