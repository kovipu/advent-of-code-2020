module Year2020.Day10 where

import Prelude
import Data.Array (mapMaybe, sort, (!!), (:), last, take, filter, length, head)
import Data.BigInt (BigInt, fromInt, toString)
import Data.Either (Either(..))
import Data.Foldable (foldl, foldr)
import Data.Int (fromString)
import Data.Maybe (Maybe(..))
import Data.String (split)
import Data.String.Pattern (Pattern(..))
import Effect (Effect)
import Effect.Class.Console (log, logShow)
import Effect.Exception.Unsafe (unsafeThrow)
import Debug (spy)

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

-- the 1st example is a bait
-- -> you can't always have two consecutive adapters missing
-- 1 3 5 6 7 10 11 12 15 16 19 (22)
-- 1 3 5 6 7 10 12 15 16 19 (22)
-- 1 3 5 7 10 11 12 15 16 19 (22)
-- 1 3 5 7 10 12 15 16 19 (22)
-- 1 3 6 7 10 11 12 15 16 19 (22)
-- 1 3 6 7 10 12 15 16 19 (22)

-- 1 3 7 10 11 12 15 16 19 (22) (illegal)
-- 1 3 7 10 12 15 16 19 (22) (illegal)

-- dynamic programming -> collect tail in acc and do shit
findArrangements :: Array Int -> Maybe { options :: BigInt, val :: Int }
findArrangements adapters =
  foldr
    ( \val acc ->
        let
          threePrevious = take 3 acc
          options =
            if threePrevious == [] then fromInt 1
            else foldl
              ( \acc n ->
                  if n.val - val <= 3 then acc + n.options else acc
              )
              (fromInt 0)
              threePrevious
        in
          [ { val, options } ] <> acc
    )
    []
    adapters
    # head

part2 :: String -> Effect Unit
part2 input = do
  let
    adapters = parse input # sort
    end = case last adapters of
      Just l -> l + 3
      Nothing -> unsafeThrow "huh"

    withEnds = 0 : adapters <> [ end ]
    result = case findArrangements withEnds of
      Just r -> r.options
      Nothing -> unsafeThrow "huh"
  log $ "Part 2 ==> " <> toString result
