module Year2020.Day06 where

import Prelude
import Data.Array (nub, length)
import Data.Foldable (foldl)
import Data.String (split, replaceAll)
import Data.String.CodeUnits (toCharArray)
import Data.String.Pattern (Pattern(..), Replacement(..))
import Effect (Effect)
import Effect.Class.Console (log)

test :: String
test =
  """abc

a
b
c

ab
ac

a
a
a
a

b"""

parseGroup :: String -> Int
parseGroup str =
  replaceAll (Pattern "\n") (Replacement "") str
    # toCharArray
    # nub
    # length

--------------------------------------------------------------------------------
part1 :: String -> Effect Unit
part1 input = do
  let
    result =
      split (Pattern "\n\n") input
        # foldl (\acc g -> acc + parseGroup g) 0
  log $ "Part 1 ==> " <> show result

--------------------------------------------------------------------------------
part2 :: String -> Effect Unit
part2 input = do
  let
    result = "<TODO>"
  log $ "Part 2 ==> " <> result
