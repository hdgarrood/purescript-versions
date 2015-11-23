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
  , version
  , runVersion
  , major
  , minor
  , preRelease
  , buildMetadata
  , isPreRelease
  , Identifier()
  , textual
  , numeric
  , showVersion
  , parseVersion
  , versionParser
  ) where

import Prelude
import Data.Either
import Data.Maybe
import Data.Ord (between)
import Data.Int (fromNumber, fromString)
import Data.String (fromCharArray, toCharArray, joinWith, stripPrefix)
import Data.Char (toLower)
import Data.List (List(..), toList, fromList, some, null)
import Data.Function (on)
import Data.Foldable
import Data.Maybe.Unsafe (fromJust)
import Control.Alt ((<|>))
import Control.Apply ((<*), (*>))
import Control.Monad (unless)
import Control.Monad.State.Class (get)
import Text.Parsing.Parser (Parser(), PState(..), ParseError(..), runParser, fail)
import Text.Parsing.Parser.Token (when, match)
import Text.Parsing.Parser.String (string)
import Text.Parsing.Parser.Combinators (sepBy, option)
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
runVersion :: forall r. (Int -> Int -> Int -> List Identifier -> List Identifier -> r) -> Version -> r
runVersion f (Version ma mi pa pre meta) = f ma mi pa pre meta

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
    , all acceptableIdentifier <<< toCharArray 
    ]
  startsWith str = isJust <<< stripPrefix str

acceptableIdentifier :: Char -> Boolean
acceptableIdentifier ch = isDigit ch || isAsciiAlpha ch || ch == '-'

showIdentifier :: Identifier -> String
showIdentifier i = case i of
  IInt x -> show x
  IStr s -> s

versionParser :: Parser (List Char) Version
versionParser = do
  maj <- nonNegativeInt
  match' '.'
  min <- nonNegativeInt
  match' '.'
  pat <- nonNegativeInt

  pre       <- option Nil (match' '-' *> identifiers)
  buildMeta <- option Nil (match' '+' *> identifiers)

  eof

  pure $ Version maj min pat pre buildMeta

  where
  identifiers = identifier `sepBy` match' '.'
  identifier = intIdent <|> textIdent
  intIdent = numeric <$> nonNegativeInt
  textIdent = do
    chars <- some (when' acceptableIdentifier)
    let str = fromCharArray (fromList chars)
    case textual str of
      Just i  -> pure i
      Nothing -> fail $ "invalid identifier: " <> str

parseVersion :: String -> Either ParseError Version
parseVersion = flip runParser versionParser <<< toList <<< toCharArray

showVersion :: Version -> String
showVersion = runVersion go
  where
  go maj min pat pre build =
    joinWith "." (map show [maj, min, pat]) <> sep "-" pre <> sep "+" build

  sep _ Nil = ""
  sep prefix lst = (prefix <>) <<< joinWith "." <<< map showIdentifier $ fromList lst

nonneg :: Int -> Int
nonneg x = if x < 0 then 0 else x

-- | Tells you whether a version is a pre-release version; that is, if it has
-- | any pre-release identifiers.
isPreRelease :: Version -> Boolean
isPreRelease = not <<< null <<< preRelease

comparePre :: List Identifier -> List Identifier -> Ordering
comparePre Nil Nil = EQ
comparePre Nil _ = GT
comparePre _ Nil = LT
comparePre (Cons x xs) (Cons y ys) = compare x y <> comparePre xs ys

instance eqVersion :: Eq Version where
  eq v1 v2 = compare v1 v2 == EQ

instance ordVersion :: Ord Version where
  compare v1 v2 =
    compareNormal v1 v2 <> comparePre' v1 v2
    where
    compareNormal = compare `on` runVersion (\ma mi pa _ _ -> [ma, mi, pa])
    comparePre'   = comparePre `on` preRelease

instance _showVersion :: Show Version where
  show v = "(fromRight (parseVersion " <> show (showVersion v) <> "))"

instance eqIdentifier :: Eq Identifier where
  eq i1 i2 = compare i1 i2 == EQ

instance ordIdentifier :: Ord Identifier where
  compare (IInt _) (IStr _) = LT
  compare (IStr _) (IInt _) = GT
  compare (IInt x) (IInt y) = compare x y
  compare (IStr x) (IStr y) = compare x y