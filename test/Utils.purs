module Test.Utils where

import Prelude
import Data.Either
import Effect
import Effect.Exception
import Effect.Console hiding (error)
import Text.Parsing.Parser (ParseError())

assertEqual :: forall a. Show a => Eq a => a -> a -> Effect Unit
assertEqual x y =
  if x == y
    then pure unit
    else throwException $ error $ show x <> " did not equal " <> show y

err :: forall a. String -> Effect a
err = throwException <<< error

assertSuccess :: forall a. Either ParseError a -> Effect a
assertSuccess =
  let onLeft = err <<< ("expected successful parse, got: " <> _) <<< show
  in either onLeft pure
