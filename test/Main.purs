module Test.Main where

import Prelude
import Effect (Effect)
import Effect.Console (log)

import Test.Haskell as Haskell
import Test.Version as Version

main :: Effect Unit
main = do
  log ">>> Data.Version"
  Version.main

  log ""

  log ">>> Data.Version.Haskell"
  Haskell.main
