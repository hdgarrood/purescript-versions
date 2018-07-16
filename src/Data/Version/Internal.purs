module Data.Version.Internal where

import Prelude hiding (when)
import Data.Int (fromString)
import Data.Char.Unicode (toLower)
import Data.String.CodeUnits (fromCharArray)
import Data.List (List(), toUnfoldable, some, null)
import Data.Maybe (fromJust)
import Control.Monad.State.Class (gets)
import Text.Parsing.Parser (Parser(), ParseState(..), fail)
import Text.Parsing.Parser.Token (when, match)
import Text.Parsing.Parser.Pos (Position(), initialPos)
import Partial.Unsafe (unsafePartial)

isDigit :: Char -> Boolean
isDigit c = '0' <= c && c <= '9'

isAsciiAlpha :: Char -> Boolean
isAsciiAlpha ch = between 'a' 'z' (toLower ch)

nonNegativeInt :: Parser (List Char) Int
nonNegativeInt = fromDigits <$> some (when lieAboutPos isDigit)
  where
  fromDigits digits = unsafePartial $ fromJust $ fromString $ fromCharArray $ toUnfoldable digits

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
