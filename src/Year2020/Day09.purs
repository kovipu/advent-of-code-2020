module Year2020.Day09 where

import Prelude
import Control.Alternative (guard)
import Data.Array (mapMaybe, mapWithIndex, find, drop, slice, (!!))
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.Int (fromString)
import Data.Maybe (Maybe(..))
import Data.Ord (min, max)
import Data.String (split)
import Data.String.Pattern (Pattern(..))
import Effect (Effect)
import Effect.Class.Console (log, logShow)
import Effect.Exception (throw)
import Effect.Exception.Unsafe (unsafeThrow)
import Partial.Unsafe (unsafePartial)
import Debug (spy)

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
-- 2 pointer technique
type Result = Either State (Array Int)

type State = { start :: Int, end :: Int, sum :: Int }

findSetSum :: State -> Array Int -> Int -> Array Int
findSetSum state nums weakness = case unsafePartial $ step state nums weakness of
  Right res -> res
  Left newState -> findSetSum newState nums weakness

step :: Partial => State -> Array Int -> Int -> Result
step { start, end, sum } nums weakness =
  let
    head = indexThrow nums start

    new = indexThrow nums end
  in
    case sum + new of
      n | n < weakness ->
        Left $ { start, end: end + 1, sum: n }
      n | n > weakness ->
        Left $ { start: start + 1, end, sum: sum - head }
      n | n == weakness ->
        Right $ slice start (end + 1) nums
      _ ->
        unsafeThrow "should not happen."

indexThrow :: forall a. Partial => Array a -> Int -> a
indexThrow arr idx = case arr !! idx of
  Just v -> v
  Nothing -> unsafeThrow "Index out of bounds."

part2 :: String -> Effect Unit
part2 input = do
  let
    nums = parse input

    preambleSize = 25

    weakness = case findWeakness nums preambleSize of
      Just n -> n
      _ -> unsafeThrow "huh"

    initialState :: State
    initialState = { start: 0, end: 0, sum: 0 }

    set = findSetSum initialState nums weakness

    minimum = foldl min 99999999 set

    maximum = foldl max 0 set

    result = minimum + maximum
  log $ "Part 2 ==> " <> show result
