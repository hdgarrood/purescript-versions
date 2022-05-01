module Test.Utils where

import Prelude
import Data.Either (Either, either)
import Effect (Effect)
import Effect.Exception (error, throw, throwException)
import Parsing (ParseError())

assertEqual :: forall a. Show a => Eq a => a -> a -> Effect Unit
assertEqual x y =
  if x == y
    then pure unit
    else throwException $ error $ show x <> " did not equal " <> show y

assertSuccess :: forall a. Either ParseError a -> Effect a
assertSuccess =
  let onLeft = throw <<< ("expected successful parse, got: " <> _) <<< show
  in either onLeft pure
