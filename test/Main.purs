module Test.Main where

import Prelude
import Effect
import Effect.Console hiding (error)

import Test.Haskell as Haskell
import Test.Version as Version
import Test.Utils

main :: Effect Unit
main = do
  log ">>> Data.Version"
  Version.main

  log ""

  log ">>> Data.Version.Haskell"
  Haskell.main
