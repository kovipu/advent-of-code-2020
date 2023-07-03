module Year2020.Day13 where

import Prelude
import Data.Array (unsafeIndex, catMaybes)
import Data.Foldable (foldl)
import Data.Int (fromString)
import Data.Maybe (fromJust)
import Data.String (split)
import Data.String.Pattern (Pattern(..))
import Data.Tuple (Tuple(..), snd)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Class.Console (log, logShow)
import Partial.Unsafe (unsafePartial)

example :: String
example =
  """939
7,13,x,x,59,x,31,19
"""

parse :: String -> Tuple Int (Array Int)
parse str =
  let
    rows = split (Pattern "\n") str
    earliest = unsafePartial $ unsafeIndex rows 0
      # fromString
      # fromJust
    buses = unsafePartial $ unsafeIndex rows 1
      # split (Pattern ",")
      # map fromString
      # catMaybes
  in
    Tuple earliest buses

--------------------------------------------------------------------------------
findDeparture :: Int -> Int -> Int
findDeparture earliest bus =
  (earliest / bus + 1) * bus

part1 :: String -> Effect Unit
part1 input = do
  let
    earliest /\ buses = parse input
    busId /\ departure = foldl
      ( \acc b ->
          let
            newDeparture = findDeparture earliest b
          in
            if newDeparture < (snd acc) then Tuple b newDeparture
            else acc
      )
      (0 /\ 99999999)
      buses
    result = (departure - earliest) * busId
  log $ "Part 1 ==> " <> show result

--------------------------------------------------------------------------------

part2 :: String -> Effect Unit
part2 input = do
  let result = "<TODO>"
  log $ "Part 2 ==> " <> result

