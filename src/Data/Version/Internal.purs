module Data.Version.Internal where

import Prelude
import Data.Either
import Data.Maybe
import Data.Ord (between)
import Data.Int (fromNumber, fromString)
import Data.Char (toLower)
import Data.String (fromCharArray, toCharArray, joinWith)
import Data.List (List(), toList, fromList, some, null)
import Data.Function (on)
import Data.Foldable
import Data.Maybe.Unsafe (fromJust)
import Control.Apply ((<*))
import Control.Monad (unless)
import Control.Monad.State.Class (get)
import Text.Parsing.Parser (Parser(), PState(..), ParseError(..), runParser, fail)
import Text.Parsing.Parser.Token (when, match)
import Text.Parsing.Parser.String (string)
import Text.Parsing.Parser.Combinators (sepBy)
import Text.Parsing.Parser.Pos (Position(), initialPos)

isDigit :: Char -> Boolean
isDigit c = '0' <= c && c <= '9'

isAsciiAlpha :: Char -> Boolean
isAsciiAlpha ch = between 'a' 'z' (toLower ch)

nonNegativeInt :: Parser (List Char) Int
nonNegativeInt = fromDigits <$> some (when lieAboutPos isDigit)
  where
  fromDigits = fromJust <<< fromString <<< fromCharArray <<< fromList

lieAboutPos :: forall a. a -> Position
lieAboutPos = const initialPos

match' :: Char -> Parser (List Char) Char
match' = match lieAboutPos

when' :: forall a. (a -> Boolean) -> Parser (List a) a
when' = when lieAboutPos

eof :: forall a. Parser (List a) Unit
eof =
  get >>= \(input :: List a) ->
    unless (null input) (fail "expected eof")
