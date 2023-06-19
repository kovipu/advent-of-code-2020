module Year2020.Day01 where

import Prelude
import Data.String (split)
import Data.String.Pattern (Pattern(..))
import Data.Array (filter, catMaybes, (!!), head, tail, findIndex)
import Control.Alternative (guard)
import Data.Int (fromString)
import Data.Maybe (Maybe(..), fromMaybe)
import Effect (Effect)
import Effect.Class.Console (log)

test :: String
test =
  """1721
979
366
299
675
1456"""

parse :: String -> Array Int
parse input =
  split (Pattern "\n") input
    # filter (_ /= "") -- test input has a newline
    # map (\n -> fromString n)
    # catMaybes -- all should be valid integers

part1 :: String -> Effect Unit
part1 input = do
  let
    report = parse input

    result =
      findSum report
        # fromMaybe (-1)
        # show
  log $ "Part 1 ==> " <> result

findSum :: Array Int -> Maybe Int
findSum report = do
  x <- head report
  xs <- tail report
  -- add x to each in xs
  let
    sums = map (_ + x) xs

    match = findIndex (_ == 2020) sums
  case match of
    -- if 2020 found -> return product
    Just idx -> xs !! idx # map (_ * x)
    -- if not found -> recurse with xs
    Nothing -> findSum xs

--------------------------------------------------------------------------------
part2 :: String -> Effect Unit
part2 input = do
  let
    report = parse input

    result = findThreeSum report # head # fromMaybe (-1) # show
  log $ "Part 2 ==> " <> result

findThreeSum :: Array Int -> Array Int
findThreeSum arr = do
  x <- arr
  y <- arr
  z <- arr
  guard $ x + y + z == 2020
  pure (x * y * z)
