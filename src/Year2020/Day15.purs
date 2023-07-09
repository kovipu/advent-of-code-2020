module Year2020.Day15 where

import Prelude
import Data.Array (init, length, last, (..))
import Data.Foldable (foldl)
import Data.FoldableWithIndex (foldlWithIndex)
import Data.String (trim, split)
import Data.String.Pattern (Pattern(..))
import Data.Int (fromString)
import Data.Maybe (Maybe(..), fromJust)
import Data.Map (Map)
import Data.Map as Map
import Effect (Effect)
import Effect.Class.Console (log, logShow)
import Partial.Unsafe (unsafePartial)
import Debug (spy)

example :: String
example =
  """0,3,6
"""

parse :: String -> Array Int
parse input = trim input
  # split (Pattern ",")
  # map (unsafePartial $ fromString >>> fromJust)

--------------------------------------------------------------------------------

findNth :: Array Int -> Int -> Int
findNth start endIdx =
  let
    prevIndices :: Map Int Int
    prevIndices = foldlWithIndex
      (\idx acc n -> Map.insert n idx acc)
      Map.empty
      (unsafePartial $ init >>> fromJust $ start)

    prevNumber = unsafePartial (last >>> fromJust) start

    startIdx = length start
  in
    foldl
      ( \{ prevNumber, prevIndices } idx ->
          -- if number in map -> idx - prevIndex
          -- if new number -> 0
          case Map.lookup prevNumber prevIndices of
            Just prevIdx ->
              { prevNumber: (idx - prevIdx - 1)
              , prevIndices: Map.insert prevNumber (idx - 1) prevIndices
              }
            Nothing ->
              { prevNumber: 0
              , prevIndices: Map.insert prevNumber (idx - 1) prevIndices
              }
      )
      { prevNumber, prevIndices }
      (startIdx .. (endIdx - 1))
      # _.prevNumber

part1 :: String -> Effect Unit
part1 input = do
  let
    start = parse input
    result = findNth start 2020
  log $ "Part 1 ==> " <> show result

--------------------------------------------------------------------------------

part2 :: String -> Effect Unit
part2 input = do
  let
    start = parse input
    result = findNth start 30000000
  log $ "Part 2 ==> " <> show result

