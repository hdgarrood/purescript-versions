-- | A Version data type corresponding to the type in Haskell's `Data.Version`
-- | module, from the `base` library (that is, the standard library).
-- |
-- | See also [the Haskell documentation](http://hackage.haskell.org/package/base-4.8.1.0/docs/Data-Version.html).
module Data.Version.Haskell where

import Prelude
import Control.Apply ((*>))
import Data.Either
import Data.List (List(..), fromList, toList, some)
import Data.String (fromCharArray, toCharArray, joinWith)
import Text.Parsing.Parser (Parser(), ParseError(), runParser)
import Text.Parsing.Parser.Combinators (sepBy, option)

import Data.Version.Internal

-- | A version consists of any number of integer components, and any number of
-- | string components.
data Version = Version (List Int) (List String)

showVersion :: Version -> String
showVersion (Version as bs) = f as <> prefix "-" (joinWith "-" (fromList bs))
  where
  f = joinWith "." <<< fromList <<< map show

  prefix _ "" = ""
  prefix p s  = p <> s

versionParser :: Parser (List Char) Version
versionParser = do
  as <- nonNegativeInt `sepBy` match' '.'
  bs <- option Nil (hyphen *> identifier `sepBy` hyphen)
  eof
  pure $ Version as bs

  where
  hyphen = match' '-'
  identifier = (fromCharArray <<< fromList) <$> someAlphaNums
  someAlphaNums = some (when' (\c -> isAsciiAlpha c || isDigit c))

parseVersion :: String -> Either ParseError Version
parseVersion = flip runParser versionParser <<< toList <<< toCharArray

instance eqVersion :: Eq Version where
  eq (Version a1 b1) (Version a2 b2) = a1 == a2 && b1 == b2

instance ordVersion :: Ord Version where
  compare (Version a1 b1) (Version a2 b2) =
    compare a1 a2 <> compare b1 b2

instance _showVersion :: Show Version where
  show (Version as bs) = "(Version " <> show as <> " " <> show bs <> ")"
