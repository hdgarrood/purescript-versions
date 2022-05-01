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
  , patch
  , preRelease
  , buildMetadata
  , isPreRelease
  , bumpMajor
  , bumpMinor
  , bumpPatch
  , Identifier()
  , textual
  , numeric
  , showVersion
  , parseVersion
  , versionParser
  ) where

import Prelude
import Data.Either (Either)
import Data.Maybe (Maybe(..), isJust)
import Data.Int (fromString)
import Data.String (joinWith, stripPrefix, Pattern(..))
import Data.String.CodeUnits (toCharArray, fromCharArray)
import Data.List (List(..), fromFoldable, toUnfoldable, some, null)
import Data.Function (on)
import Data.Foldable (all)
import Control.Alt ((<|>))
import Parsing (Parser(), ParseError(), runParser, fail)
import Parsing.Combinators (sepBy, option)

import Data.Version.Internal (eof, isAsciiAlpha, isDigit, match', nonNegativeInt, when')

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

-- | Bump the major version, and discard any prerelease identifiers and build
-- | metadata.
bumpMajor :: Version -> Version
bumpMajor v = version (major v + 1) 0 0 Nil Nil

-- | Bump the minor version, and discard any prerelease identifiers and build
-- | metadata.
bumpMinor :: Version -> Version
bumpMinor v = version (major v) (minor v + 1) 0 Nil Nil

-- | Bump the patch version, and discard any prerelease identifiers and build
-- | metadata.
bumpPatch :: Version -> Version
bumpPatch v = version (major v) (minor v) (patch v + 1) Nil Nil

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
  ok x = all (_ $ x)
    [ not <<< isJust <<< fromString -- check that it isn't a number
    , not <<< startsWith "0"
    , all acceptableIdentifier <<< toCharArray
    ]
  startsWith prefix = isJust <<< stripPrefix (Pattern prefix)

acceptableIdentifier :: Char -> Boolean
acceptableIdentifier ch = isDigit ch || isAsciiAlpha ch || ch == '-'

showIdentifier :: Identifier -> String
showIdentifier i = case i of
  IInt x -> show x
  IStr s -> s

versionParser :: Parser (List Char) Version
versionParser = do
  maj <- nonNegativeInt
  _ <- match' '.'
  min <- nonNegativeInt
  _ <- match' '.'
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
    let str = fromCharArray (toUnfoldable chars)
    case textual str of
      Just i  -> pure i
      Nothing -> fail $ "invalid identifier: " <> str

parseVersion :: String -> Either ParseError Version
parseVersion = flip runParser versionParser <<< fromFoldable <<< toCharArray

showVersion :: Version -> String
showVersion = runVersion go
  where
  go maj min pat pre build =
    joinWith "." (map show [maj, min, pat]) <> sep "-" pre <> sep "+" build

  sep _ Nil = ""
  sep prefix lst = (prefix <> _) <<< joinWith "." <<< map showIdentifier $ toUnfoldable lst

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
comparePre (Cons x xs) (Cons y ys) = compare x y <> helper xs ys
  where
  helper Nil Nil = EQ
  helper Nil _ = LT
  helper _ Nil = GT
  helper (Cons x' xs') (Cons y' ys') = compare x' y' <> helper xs' ys'

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

instance _showIdentifier :: Show Identifier where
  show (IInt i) = "(numeric " <> show i <> ")"
  show (IStr s) = "(fromJust (textual " <> show s <> "))"
