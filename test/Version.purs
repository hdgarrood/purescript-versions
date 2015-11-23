module Test.Version where

import Prelude
import Data.Tuple
import Data.List
import Data.Either
import Data.Foldable
import Data.Maybe.Unsafe (fromJust)
import Control.Monad.Eff
import Control.Monad.Eff.Exception
import Control.Monad.Eff.Console hiding (error)
import Text.Parsing.Parser (ParseError())

import Data.Version
import Test.Utils

testVersions :: Array (Tuple String Version)
testVersions =
  [ Tuple "0.0.0"         $ v 0 0 0
  , Tuple "0.0.1"         $ v 0 0 1
  , Tuple "6.3.4"         $ v 6 3 4
  , Tuple "13.26.346"     $ v 13 26 346
  , Tuple "1.2.3-alpha.5" $ v' 1 2 3 (toList [t "alpha", n 5]) Nil
  , Tuple "1.2.3-6+12"    $ v' 1 2 3 (toList [n 6]) (toList [n 12])
  ]
  where
  v a b c = version a b c Nil Nil
  v' = version
  t = fromJust <<< textual
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
      Left _  -> return unit
