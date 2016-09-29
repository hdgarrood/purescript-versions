module Test.Haskell where

import Prelude
import Data.Tuple
import Data.List
import Data.Either
import Data.Foldable
import Control.Monad.Eff
import Control.Monad.Eff.Exception
import Control.Monad.Eff.Console hiding (error)

import Data.Version.Haskell
import Test.Utils

testVersions :: Array (Tuple String Version)
testVersions =
  [ Tuple "0.0.0"         $ v [0,0,0] []
  , Tuple "0.0.1.1"       $ v [0,0,1,1] []
  , Tuple "6.3.4"         $ v [6,3,4] []
  , Tuple "13.26.346"     $ v [13,26,346] []
  , Tuple "1.2.3-alpha-5" $ v [1,2,3] ["alpha", "5"]
  ]
  where
  v as bs = Version (fromFoldable as) (fromFoldable bs)

invalidVersions :: Array String
invalidVersions =
  [ "lol"
  , "0.1.2.lol"
  , "0lol.1.2"
  , "🐱🐱🐱"
  , "33.1.20."
  , "."
  , "13."
  , ".6"
  ]

main :: Eff (err :: EXCEPTION, console :: CONSOLE) Unit
main = do
  log "parseVersion, showVersion are inverses"
  for_ testVersions \(Tuple str vers) -> do
    log $ "  " <> str
    parsed <- assertSuccess $ parseVersion str
    assertEqual parsed vers
    assertEqual str (showVersion vers)

  log "invalid versions produce parse errors"
  for_ invalidVersions $ \str -> do
    log $ "  " <> str
    case parseVersion str of
      Right v -> err $ "expected parse error, got: " <> show v
      Left _  -> pure unit
