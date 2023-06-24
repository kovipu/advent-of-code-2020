module Year2020.Day03 where

import Prelude
import Data.BigInt (BigInt, fromInt, toString)
import Data.Foldable (foldl)
import Data.FoldableWithIndex (foldlWithIndex)
import Data.Array (filter)
import Data.String (split, length)
import Data.String.Pattern (Pattern(..))
import Data.String.Unsafe (charAt)
import Effect (Effect)
import Effect.Class.Console (log)

test :: String
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
            if treeAtCoords (y * 3) row then
              acc + 1
            else
              acc
        )
        0
        map
  log $ "Part 1 ==> " <> show result

treeAtCoords :: Int -> String -> Boolean
treeAtCoords x row =
  let
    xc = x `mod` (length row)

    location = charAt xc row
  in
    location == '#'

--------------------------------------------------------------------------------
type Slope
  = { right :: Int
    , down :: Int
    }

part2 :: String -> Effect Unit
part2 input = do
  let
    map = parse input

    slopes :: Array Slope
    slopes =
      [ { right: 1, down: 1 }
      , { right: 3, down: 1 }
      , { right: 5, down: 1 }
      , { right: 7, down: 1 }
      , { right: 1, down: 2 }
      ]

    result :: BigInt
    result = foldl (\acc slope -> acc * fromInt (findTrees map slope)) (fromInt 1) slopes
  log $ "Part 2 ==> " <> toString result

findTrees :: TreeMap -> Slope -> Int
findTrees tmap { right, down } =
  foldlWithIndex
    ( \y acc row ->
        ( let
            -- skip even coords when going down by 2.
            skipEven = down == 2 && y `mod` 2 /= 0

            x = y * right / down
          in
            if not skipEven && treeAtCoords x row then
              acc + 1
            else
              acc
        )
    )
    0
    tmap
