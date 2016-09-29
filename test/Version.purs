module Test.Version where

import Prelude
import Data.Tuple
import Data.List hiding (sort)
import Data.Either
import Data.Foldable
import Data.Traversable
import Data.Array (sort)
import Data.Maybe (fromJust)
import Control.Monad.Eff
import Control.Monad.Eff.Exception
import Control.Monad.Eff.Console hiding (error)
import Partial.Unsafe

import Data.Version
import Test.Utils

testVersions :: Array (Tuple String Version)
testVersions = normals <> pres <> metas
  where
  normals =
    [ Tuple "0.0.0"         $ v 0 0 0
    , Tuple "0.0.1"         $ v 0 0 1
    , Tuple "6.3.4"         $ v 6 3 4
    , Tuple "13.26.346"     $ v 13 26 346
    ]

  v a b c = version a b c Nil Nil

  pres =
    [ Tuple "1.0.0-alpha"      $ v1 (fromFoldable [t "alpha"])                Nil
    , Tuple "1.0.0-alpha.1"    $ v1 (fromFoldable [t "alpha", n 1])           Nil
    , Tuple "1.0.0-0.3.7"      $ v1 (fromFoldable [n 0, n 3, n 7])            Nil
    , Tuple "1.0.0-x.7.z.926"  $ v1 (fromFoldable [t "x", n 7, t "z", n 926]) Nil
    ]

  metas =
    [ Tuple "1.0.0-a+12.23"  $ v1 (fromFoldable [t "a"])     (fromFoldable [n 12, n 23])
    , Tuple "1.0.0+hello"    $ v1 Nil                  (fromFoldable [t "hello"])
    , Tuple "1.0.0-alpha+12" $ v1 (fromFoldable [t "alpha"]) (fromFoldable [n 12])
    ]

  v1 = version 1 0 0
  t s = unsafePartial $ fromJust $ textual s
  n = numeric

invalidVersions :: Array String
invalidVersions =
  [ "lol"
  , "0.1.2.3"
  , "0.1.2.lol"
  , "0lol.1.2"
  , "🐱🐱🐱"
  , "33.1.20."
  , "."
  , "13."
  , ".6"
  ]

-- Taken from the semver spec. These should be in increasing order of
-- precedence.
preReleaseVersions :: Array String
preReleaseVersions =
  [ "1.0.0-alpha"
  , "1.0.0-alpha.1"
  , "1.0.0-alpha.beta"
  , "1.0.0-beta"
  , "1.0.0-beta.2"
  , "1.0.0-beta.11"
  , "1.0.0-rc.1"
  , "1.0.0"
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

  log "pre-release versions are ordered correctly"
  parsedVersions <- assertSuccess $ traverse parseVersion preReleaseVersions
  assertEqual parsedVersions (sort parsedVersions)
