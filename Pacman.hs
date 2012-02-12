module Pacman where

import Alpm.Base
import Alpm.Database

pacmanDBs :: Alpm [DB]
pacmanDBs = do
    db1 <- registerDB "testing"
    db2 <- registerDB "core"
    db3 <- registerDB "extra"
    db4 <- registerDB "community-testing"
    db5 <- registerDB "community"
    db6 <- registerDB "haskell"
    return [ db1, db2, db3, db4, db5, db6 ]
