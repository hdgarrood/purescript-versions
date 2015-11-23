module Test.Main where

import Prelude
import Control.Monad.Eff.Console hiding (error)

import qualified Test.Haskell as Haskell
import qualified Test.Version as Version

main = do
  log ">>> Data.Version"
  Test.Version.main

  log ""

  log ">>> Data.Version.Haskell"
  Test.Haskell.main
