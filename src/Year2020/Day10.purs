module Year2020.Day10 where

import Prelude
import Data.Array (mapMaybe, sort)
import Data.Foldable (foldl)
import Data.Int (fromString)
import Data.String (split)
import Data.String.Pattern (Pattern(..))
import Effect (Effect)
import Effect.Class.Console (log, logShow)

example :: String
example =
  """16
10
15
5
1
11
7
19
6
12
4"""

parse :: String -> Array Int
parse str =
  split (Pattern "\n") str
    # mapMaybe fromString

--------------------------------------------------------------------------------
findDifferences :: Array Int -> Int
findDifferences adapters =
  foldl
    ( \acc@{ ones, threes, prev } adapter ->
        case adapter - prev of
          3 -> acc { threes = threes + 1, prev = adapter }
          1 -> acc { ones = ones + 1, prev = adapter }
          _ -> acc { prev = adapter }
    )
    { ones: 0, threes: 1, prev: 0 }
    adapters
    # _.ones * _.threes

part1 :: String -> Effect Unit
part1 input = do
  let
    adapters = parse input # sort
    result = findDifferences adapters
  log $ "Part 1 ==> " <> show result

--------------------------------------------------------------------------------
part2 :: String -> Effect Unit
part2 input = do
  let
    result = "<TODO>"
  log $ "Part 2 ==> " <> result
