-- | This module defines a `Version` data type, for representing software
-- | versions, according to the [Semantic Versioning](http://semver.org)
-- | specification. To summarize, a `Version` consists of:
-- |
-- | * a MAJOR, MINOR, and a PATCH component, all of which are nonnegative
-- |   integers.
-- | * optionally, a list of pre-release identifiers, consisting of ASCII
-- |   letters, numbers, and hyphens, and which is separated from the three
-- |   main components with a hyphen.
-- | * optionally, build metadata, consisting of ASCII letters, numbers, and
-- |   hyphens, and which is separated from the rest of the version with a plus
-- |   symbol.
-- |
-- | Note that, according to the semver spec, version precedence must ignore
-- | any build metadata. Therefore, the `Ord` instance ignores the build
-- | metadata. In order to have the `Eq` instance agree with the `Ord`
-- | instance, the `Eq` instance ignores build metadata too.

module Data.Version
  ( Version()
  , Identifier()
  , major
  , minor
  , preRelease
  , buildMetadata
  , runVersion
  -- , showVersion
  -- , parseVersion
  -- , versionParser
  ) where

import Prelude
import Data.Either
import Data.Maybe
import Data.Ord (between)
import Data.Int (fromNumber, fromString)
import Data.String (fromCharArray, toCharArray, joinWith, stripPrefix)
import Data.Char (toLower)
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

import Data.Version.Internal

-- | A semver version.
data Version
  = Version Int Int Int (List Identifier) (List Identifier)

-- | Smart constructor for versions. Negative integer components will be
-- | replaced with zeroes.
version :: Int -> Int -> Int -> List Identifier -> List Identifier -> Version
version ma mi pa pre meta =
  Version (nonneg ma) (nonneg mi) (nonneg pa) pre meta

-- | Unpack a version. Useful for pattern matching.
-- |
-- | The reason we have this function instead of exporting the `Version`
-- | constructor is that in this way we can guarantee that `Version` values are
-- | always valid.
runVersion :: forall r. Version -> (Int -> Int -> Int -> List Identifier -> List Identifier -> r) -> r
runVersion (Version ma mi pa pre meta) f = f ma mi pa pre meta

major :: Version -> Int
major (Version x _ _ _ _) = x

minor :: Version -> Int
minor (Version _ x _ _ _) = x

patch :: Version -> Int
patch (Version _ _ x _ _) = x

preRelease :: Version -> List Identifier
preRelease (Version _ _ _ x _) = x

buildMetadata :: Version -> List Identifier
buildMetadata (Version _ _ _ _ x) = x

data Identifier
  = IInt Int
  | IStr String

-- | Construct a numeric identifier.
numeric :: Int -> Identifier
numeric = IInt <<< nonneg

-- | Construct a textual identifier.
textual :: String -> Maybe Identifier
textual str =
  if ok str then Just (IStr str) else Nothing
  where
  ok x = all ($ x)
    [ not <<< isJust <<< fromString -- check that it isn't a number
    , not <<< startsWith "0"
    , all acceptable <<< toCharArray 
    ]
  acceptable ch = isDigit ch || between 'a' 'z' (toLower ch) || ch == '-'
  startsWith str = isJust <<< stripPrefix str

showIdentifier :: Identifier -> String
showIdentifier i = case i of
  IInt x -> show x
  IStr s -> s

-- versionParser :: Parser (List Char) Version
-- versionParser = Version <$> (int `sepBy` dot) <* eof

-- parseVersion :: String -> Either ParseError Version
-- parseVersion = flip runParser versionParser <<< toList <<< toCharArray

nonneg :: Int -> Int
nonneg x = if x < 0 then 0 else x

-- instance eqVersion :: Eq Version where
--   eq v1 v2 = compare v1 v2 == EQ

-- instance ordVersion :: Ord Version where
--   compare = compare `on` components
--     where
--     components v = map ($ v) [major, minor, patch] preRelease]

-- instance _showVersion :: Show Version where
--   show (Version xs) = "(Version " <> show xs <> ")"
