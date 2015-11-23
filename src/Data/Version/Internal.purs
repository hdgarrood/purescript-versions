module Data.Version.Internal where

import Prelude
import Data.Ord (between)
import Data.Int (fromString)
import Data.Char (toLower)
import Data.String (fromCharArray)
import Data.List (List(), fromList, some, null)
import Data.Maybe.Unsafe (fromJust)
import Control.Monad (unless)
import Control.Monad.State.Class (get)
import Text.Parsing.Parser (Parser(), fail)
import Text.Parsing.Parser.Token (when, match)
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
