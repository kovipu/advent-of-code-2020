module Year2020.Day05 where

import Prelude
import Data.Array ((!!), filter)
import Data.Foldable (foldl)
import Data.Int (pow)
import Data.Maybe (Maybe(..))
import Data.String (split)
import Data.String.Unsafe (charAt)
import Data.String.Pattern (Pattern(..))
import Effect (Effect)
import Effect.Class.Console (log, logShow)

test :: String
test =
  """BFFFBBFRRR
FFFBBBFRRR
BBFFBBFRLL"""

--------------------------------------------------------------------------------
part1 :: String -> Effect Unit
part1 input = do
  let
    result =
      split (Pattern "\n") input
        # filter (_ /= "")
        # foldl
            ( \acc p ->
                let
                  newId = (parseSeat p).id
                in
                  if newId > acc then
                    newId
                  else
                    acc
            )
            0
  log $ "Part 1 ==> " <> show result

type Seat
  = { row :: Int
    , column :: Int
    , id :: Int
    }

parseSeat :: String -> Seat
parseSeat str =
  let
    -- first 7 characters make out the row
    bit0 = rowValue str 0

    bit1 = rowValue str 1

    bit2 = rowValue str 2

    bit3 = rowValue str 3

    bit4 = rowValue str 4

    bit5 = rowValue str 5

    bit6 = rowValue str 6

    row = bit6 + bit5 + bit4 + bit3 + bit2 + bit1 + bit0

    -- next 3 characters are the column
    bit7 = colValue str 7

    bit8 = colValue str 8

    bit9 = colValue str 9

    column = bit7 + bit8 + bit9

    -- id maths
    id = 8 * row + column
  in
    { row: row
    , column: column
    , id: id
    }

rowValue :: String -> Int -> Int
rowValue str idx =
  if charAt idx str == 'B' then
    pow 2 (6 - idx)
  else
    0

colValue :: String -> Int -> Int
colValue str idx =
  if charAt idx str == 'R' then
    pow 2 (9 - idx)
  else
    0

--------------------------------------------------------------------------------
part2 :: String -> Effect Unit
part2 input = do
  let
    result = "<TODO>"
  log $ "Part 2 ==> " <> result
