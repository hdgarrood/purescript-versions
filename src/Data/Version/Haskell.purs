-- | A Version data type corresponding to the one in Haskell's `Data.Version`
-- | module in its `base` library.
module Data.Version.Haskell where

import Prelude
import Data.List
import Data.String (joinWith)

data Version = Version (List Int) (List String)

showVersion :: Version -> String
showVersion (Version ints strings) = f ints <> "-" <> g strings
  where
  f = joinWith "." <<< fromList <<< map show
  g = joinWith "-" <<< fromList
