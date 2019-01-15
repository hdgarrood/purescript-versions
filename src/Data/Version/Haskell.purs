-- | A Version data type corresponding to the type in Haskell's `Data.Version`
-- | module, from the `base` library (that is, the Haskell standard library).
-- |
-- | See also [the Haskell documentation](http://hackage.haskell.org/package/base-4.8.1.0/docs/Data-Version.html).
-- |
-- | You might need this module in order to deal with PureScript compiler
-- | versions, as they do not always have 3 components, and therefore will not
-- | work with the `Data.Version` module in this package. In most other cases,
-- | though, you should probably be using `Data.Version`.
module Data.Version.Haskell where

import Prelude 
import Data.Either (Either)
import Data.List (List(..), toUnfoldable, fromFoldable, some)
import Data.String (joinWith)
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Text.Parsing.Parser (Parser(), ParseError(), runParser)
import Text.Parsing.Parser.Combinators (sepBy, sepBy1, option)

import Data.Version.Internal (eof, match', nonNegativeInt, isDigit, isAsciiAlpha, when') 

-- | A version consists of any number of integer components, and any number of
-- | string components.
data Version = Version (List Int) (List String)

showVersion :: Version -> String
showVersion (Version as bs) = f as <> prefix "-" (joinWith "-" (toUnfoldable bs))
  where
  f = joinWith "." <<< toUnfoldable <<< map show

  prefix _ "" = ""
  prefix p s  = p <> s

versionParser :: Parser (List Char) Version
versionParser = do
  as <- nonNegativeInt `sepBy1` match' '.'
  bs <- option Nil (hyphen *> identifier `sepBy` hyphen)
  eof
  pure $ Version as bs

  where
  hyphen = match' '-'
  identifier = (fromCharArray <<< toUnfoldable) <$> someAlphaNums
  someAlphaNums = some (when' (\c -> isAsciiAlpha c || isDigit c))

parseVersion :: String -> Either ParseError Version
parseVersion = flip runParser versionParser <<< fromFoldable <<< toCharArray

instance eqVersion :: Eq Version where
  eq (Version a1 b1) (Version a2 b2) = a1 == a2 && b1 == b2

instance ordVersion :: Ord Version where
  compare (Version a1 b1) (Version a2 b2) =
    compare a1 a2 <> compare b1 b2

instance _showVersion :: Show Version where
  show (Version as bs) = "(Version " <> show as <> " " <> show bs <> ")"
