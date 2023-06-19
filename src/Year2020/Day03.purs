module Year2020.Day03 where

import Prelude
import Data.FoldableWithIndex (foldlWithIndex)
import Data.Array (filter)
import Data.Maybe (Maybe)
import Data.String (split, length)
import Data.String.CodeUnits (toCharArray)
import Data.String.Pattern (Pattern(..))
import Data.String.Unsafe (charAt)
import Effect (Effect)
import Effect.Class.Console (log, logShow)

test =
  """..##.......
#...#...#..
.#....#..#.
..#.#...#.#
.#...##..#.
..#.##.....
.#.#.#....#
.#........#
#.##...#...
#...##....#
.#..#...#.#
"""

type TreeMap
  = Array String

parse :: String -> TreeMap
parse input =
  split (Pattern "\n") input
    # filter (_ /= "")

--------------------------------------------------------------------------------
part1 :: String -> Effect Unit
part1 input = do
  let
    map = parse input

    result =
      foldlWithIndex
        ( \y acc row ->
            if treeAtCoords y row then
              acc + 1
            else
              acc
        )
        0
        map
  log $ "Part 1 ==> " <> show result

treeAtCoords :: Int -> String -> Boolean
treeAtCoords y row =
  let
    x = (y * 3) `mod` (length row)

    location = charAt x row
  in
    location == '#'

--------------------------------------------------------------------------------
part2 :: String -> Effect Unit
part2 input = do
  let
    result = "<TODO>"
  log $ "Part 2 ==> " <> result
