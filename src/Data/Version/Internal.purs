module Data.Version.Internal where

import Prelude hiding (when)

import Control.Monad.State.Class (gets)
import Data.Char.Unicode (toLower)
import Data.Int (fromString)
import Data.List (List, toUnfoldable, some, null)
import Data.Maybe (maybe)
import Data.String.CodeUnits (fromCharArray)
import Text.Parsing.Parser (Parser, ParseState(..), fail)
import Text.Parsing.Parser.Pos (Position, initialPos)
import Text.Parsing.Parser.Token (when, match)

isDigit :: Char -> Boolean
isDigit c = '0' <= c && c <= '9'

isAsciiAlpha :: Char -> Boolean
isAsciiAlpha ch = between 'a' 'z' (toLower ch)

nonNegativeInt :: Parser (List Char) Int
nonNegativeInt =
  intFromList <$> some (when lieAboutPos isDigit) >>= maybe (fail "invalid 32-bit integer") pure
  where
  intFromList = fromString <<< fromCharArray <<< toUnfoldable

lieAboutPos :: forall a. a -> Position
lieAboutPos = const initialPos

match' :: Char -> Parser (List Char) Char
match' = match lieAboutPos

when' :: forall a. (a -> Boolean) -> Parser (List a) a
when' = when lieAboutPos

eof :: forall a. Parser (List a) Unit
eof =
  gets (\(ParseState input _ _) -> input) >>= \(input :: List a) ->
    unless (null input) (fail "expected eof")
