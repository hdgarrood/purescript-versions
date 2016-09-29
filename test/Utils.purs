module Test.Utils where

import Prelude
import Data.Either
import Control.Monad.Eff
import Control.Monad.Eff.Exception
import Control.Monad.Eff.Console hiding (error)
import Text.Parsing.Parser (ParseError())

type EffT a =
  Eff (err :: EXCEPTION, console :: CONSOLE) a

assertEqual :: forall a. (Show a, Eq a) => a -> a -> EffT Unit
assertEqual x y =
  if x == y
    then pure unit
    else throwException $ error $ show x <> " did not equal " <> show y

err :: forall a. String -> EffT a
err = throwException <<< error

assertSuccess :: forall a. Either ParseError a -> EffT a
assertSuccess =
  let onLeft = err <<< ("expected successful parse, got: " <> _) <<< show
  in either onLeft pure
