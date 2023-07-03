module Year2020.Day13 where

import Prelude
import Data.Array (catMaybes, reverse, snoc, sortWith, unsafeIndex)
import Data.BigInt (BigInt, fromInt, toString)
import Data.Foldable (foldl)
import Data.FoldableWithIndex (foldlWithIndex)
import Data.Int (fromString)
import Data.Maybe (Maybe(..), fromJust)
import Data.String (split)
import Data.String.Pattern (Pattern(..))
import Data.Tuple (Tuple(..), snd)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Class.Console (log)
import Partial.Unsafe (unsafePartial)

example :: String
example =
  """939
7,13,x,x,59,x,31,19
"""

example2 :: String
example2 =
  """
1789,37,47,1889
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

parse2 :: String -> Array (Tuple BigInt BigInt)
parse2 str =
  unsafePartial $ unsafeIndex (split (Pattern "\n") str) 1
    # split (Pattern ",")
    # foldlWithIndex
        ( \idx acc b -> case fromString b of
            Just n -> snoc acc
              $ Tuple
                  (fromInt (idx `mod` n))
                  (fromInt n)
            Nothing -> acc
        )
        []

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

findSubsequentDepartures :: Array (Tuple BigInt BigInt) -> BigInt
findSubsequentDepartures buses =
  let
    sorted = sortWith snd buses # reverse
  in
    foldl
      ( \{ start, step } (idx /\ id) ->
          { start: findShit start step id idx
          , step: id * step
          }
      )
      { start: fromInt 0, step: fromInt 1 }
      sorted
      # _.start

findShit :: BigInt -> BigInt -> BigInt -> BigInt -> BigInt
findShit start step id idx =
  if start `mod` id == (if idx == fromInt 0 then idx else id - idx) then start
  else findShit (start + step) step id idx

part2 :: String -> Effect Unit
part2 input = do
  let
    buses = parse2 input
    result = findSubsequentDepartures buses
  log $ "Part 2 ==> " <> toString result

