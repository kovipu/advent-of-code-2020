module Year2020.Day01 where

import Prelude
import Data.Function (($))
import Data.Semigroup ((<>))
import Data.Unit (Unit)
import Data.String (split)
import Data.String.Pattern (Pattern(..))
import Data.Array (filter, init, catMaybes, (!!), head, tail, findIndex)
import Data.Int.Parse (parseInt, toRadix)
import Data.Maybe (Maybe(..), fromMaybe)
import Effect (Effect)
import Effect.Class.Console (log, logShow)

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
    # map (\n -> parseInt n (toRadix 10))
    # catMaybes -- all should be valid integers

part1 :: String -> Effect Unit
part1 input = do
  let
    report = parse input
  let
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
  -- did we get 2020?
  let
    match = findIndex (_ == 2020) sums
  -- if not -> recurse with xs
  case match of
    Just idx -> xs !! idx >>= \n -> Just (x * n)
    Nothing -> findSum xs

--------------------------------------------------------------------------------
part2 :: String -> Effect Unit
part2 input = do
  let
    result = "<TODO>"
  log $ "Part 2 ==> " <> result
