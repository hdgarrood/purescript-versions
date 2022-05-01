module Data.Version.Internal where

import Prelude hiding (when)

import Data.CodePoint.Unicode (toLowerSimple)
import Data.Int (fromString)
import Data.List (List, toUnfoldable, some)
import Data.Maybe (maybe)
import Data.String (codePointFromChar)
import Data.String.CodeUnits (fromCharArray)
import Parsing (Parser, Position, fail, initialPos)
import Parsing.Token (when, match)

isDigit :: Char -> Boolean
isDigit c = '0' <= c && c <= '9'

isAsciiAlpha :: Char -> Boolean
isAsciiAlpha =
  between (codePointFromChar 'a') (codePointFromChar 'z') <<< toLowerSimple <<< codePointFromChar

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
