module Data.Version.Internal where

import Prelude
import Data.Either
import Data.Maybe
import Data.Int (fromNumber, fromString)
import Data.String (fromCharArray, toCharArray, joinWith)
import Data.List (List(), toList, fromList, some, null)
import Data.Function (on)
import Data.Foldable
import Data.Maybe.Unsafe (fromJust)
import Control.Apply ((<*))
import Control.Monad (unless)
import Control.Monad.State.Class (get)
import Text.Parsing.Parser (Parser(), PState(..), ParseError(..), runParser, fail)
import Text.Parsing.Parser.Token (when)
import Text.Parsing.Parser.String (string)
import Text.Parsing.Parser.Combinators (sepBy)
import Text.Parsing.Parser.Pos (Position(), initialPos)

isDigit :: Char -> Boolean
isDigit c = '0' <= c && c <= '9'

int :: Parser (List Char) Int
int = (fromJust <<< fromString <<< fromCharArray <<< fromList) <$> some (when lieAboutPos isDigit)

lieAboutPos :: forall a. a -> Position
lieAboutPos = const initialPos

dot :: Parser (List Char) Unit
dot = void $ when lieAboutPos (== '.')

eof :: forall a. Parser (List a) Unit
eof =
  get >>= \(input :: List a) ->
    unless (null input) (fail "expected eof")
