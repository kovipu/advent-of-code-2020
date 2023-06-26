module Year2020.Day09 where

import Prelude
import Control.Alternative (guard)
import Data.Array (mapMaybe, mapWithIndex, find, drop, slice)
import Data.Int (fromString)
import Data.Maybe (Maybe(..))
import Data.String (split)
import Data.String.Pattern (Pattern(..))
import Effect (Effect)
import Effect.Class.Console (log, logShow)
import Effect.Exception (throw)

example :: String
example =
  """35
20
15
25
47
40
62
55
65
95
102
117
150
182
127
219
299
277
309
576"""

parse :: String -> Array Int
parse str =
  split (Pattern "\n") str
    # mapMaybe fromString

--------------------------------------------------------------------------------
findWeakness :: Array Int -> Int -> Maybe Int
findWeakness nums preambleSize =
  let
    searchable =
      mapWithIndex (\idx val -> { idx, val }) nums
        # drop preambleSize
  in
    find
      ( \{ idx, val } ->
          let
            preamble = slice (idx - preambleSize) idx nums
          in
            findSum val preamble == []
      )
      searchable
      # map _.val

findSum :: Int -> Array Int -> Array Int
findSum n preamble = do
  a <- preamble
  b <- preamble
  guard $ a + b == n
  pure n

part1 :: String -> Effect Unit
part1 input = do
  let
    nums = parse input

    preambleSize = 25
  result <- case findWeakness nums preambleSize of
    Just n -> pure n
    _ -> throw "huh"
  log $ "Part 1 ==> " <> show result

--------------------------------------------------------------------------------
part2 :: String -> Effect Unit
part2 input = do
  let
    result = "<TODO>"
  log $ "Part 2 ==> " <> result
