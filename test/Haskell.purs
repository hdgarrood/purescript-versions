module Test.Haskell where

import Prelude

import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.List (fromFoldable)
import Data.List.NonEmpty as NonEmptyList
import Data.Maybe (fromJust)
import Data.Tuple (Tuple(..))
import Data.Version.Haskell (Version(..), parseVersion, showVersion)
import Effect (Effect)
import Effect.Console (log)
import Effect.Exception (throw)
import Partial.Unsafe (unsafePartial)
import Test.Utils (assertEqual, assertSuccess)

testVersions :: Array (Tuple String Version)
testVersions =
  [ Tuple "0.0.0"         $ v [0,0,0] []
  , Tuple "0.0.1.1"       $ v [0,0,1,1] []
  , Tuple "6.3.4"         $ v [6,3,4] []
  , Tuple "13.26.346"     $ v [13,26,346] []
  , Tuple "1.2.3-alpha-5" $ v [1,2,3] ["alpha", "5"]
  , Tuple "23.1"          $ v [23, 1] []
  , Tuple "12"            $ v [12] []
  ]
  where
  v as bs = Version (unsafePartial fromJust $ NonEmptyList.fromFoldable as) (fromFoldable bs)

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
  , ""
  ]

main :: Effect Unit
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
      Right v -> throw $ "expected parse error, got: " <> show v
      Left _  -> pure unit
