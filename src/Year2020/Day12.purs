module Year2020.Day12 where

import Prelude
import Data.Array (filter)
import Data.Foldable (foldl)
import Data.Int (fromString)
import Data.Ord (abs)
import Data.Maybe (fromJust)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.String (split, take, drop)
import Data.String.Pattern (Pattern(..))
import Debug (spy)
import Effect (Effect)
import Effect.Class.Console (log, logShow)
import Effect.Exception.Unsafe (unsafeThrow)
import Partial.Unsafe (unsafePartial)

example :: String
example =
  """F10
N3
F7
R90
F11
"""

data Instruction
  = CardinalMove Cardinal Int
  | RelativeMove Movement Int

derive instance genericInstruction :: Generic Instruction _

instance showInstruction :: Show Instruction where
  show = genericShow

data Cardinal = North | South | East | West

derive instance genericCardinal :: Generic Cardinal _

instance showCardinal :: Show Cardinal where
  show = genericShow

data Movement = Left | Right | Forward

derive instance genericMovement :: Generic Movement _

instance showMovement :: Show Movement where
  show = genericShow

parse :: String -> Array Instruction
parse str =
  split (Pattern "\n") str
    # filter (_ /= "")
    # map (\l -> unsafePartial $ parseInst l)

parseInst :: Partial => String -> Instruction
parseInst str =
  let
    op = take 1 str
    value = drop 1 str # fromString # fromJust
  in
    case op of
      "N" -> CardinalMove North value
      "S" -> CardinalMove South value
      "E" -> CardinalMove East value
      "W" -> CardinalMove West value
      "L" -> RelativeMove Left value
      "R" -> RelativeMove Right value
      "F" -> RelativeMove Forward value
      _ -> unsafeThrow "Invalid operation."

--------------------------------------------------------------------------------
type State = { x :: Int, y :: Int, direction :: Cardinal }

step :: State -> Instruction -> State
step st@{ x, y, direction } inst =
  case inst of
    CardinalMove North val -> st { y = y + val }
    CardinalMove South val -> st { y = y - val }
    CardinalMove East val -> st { x = x + val }
    CardinalMove West val -> st { x = x - val }
    RelativeMove Left val -> st { direction = rotate direction (-1 * val) }
    RelativeMove Right val -> st { direction = rotate direction val }
    RelativeMove Forward val -> case direction of
      East -> st { x = x + val }
      South -> st { y = y - val }
      West -> st { x = x - val }
      North -> st { y = y + val }

rotate :: Cardinal -> Int -> Cardinal
rotate direction value =
  let
    current = case direction of
      East -> 0
      South -> 90
      West -> 180
      North -> 270
    new = (current + value) `mod` 360
  in
    case new of
      (-270) -> South
      (-180) -> West
      (-90) -> North
      0 -> East
      90 -> South
      180 -> West
      270 -> North
      _ -> unsafeThrow "huh"

part1 :: String -> Effect Unit
part1 input = do
  let
    instructions = parse input
    initialState = { x: 0, y: 0, direction: East }
    { x, y } = foldl step initialState instructions
    result = abs x + abs y
  log $ "Part 1 ==> " <> show result

--------------------------------------------------------------------------------

type State2 = { x :: Int, y :: Int, wx :: Int, wy :: Int }

step2 :: State2 -> Instruction -> State2
step2 st@{ x, y, wx, wy } inst =
  case inst of
    CardinalMove North val -> st { wy = wy + val }
    CardinalMove South val -> st { wy = wy - val }
    CardinalMove East val -> st { wx = wx + val }
    CardinalMove West val -> st { wx = wx - val }
    RelativeMove Left val -> rotate2 st val
    RelativeMove Right val -> rotate2 st (-1 * val)
    RelativeMove Forward val -> st { x = x + (val * wx), y = y + (val * wy) }

rotate2 :: State2 -> Int -> State2
rotate2 st@{ x, y, wx, wy } angle =
  let
    c = cos angle
    s = sin angle
    qx = wx * c - wy * s
    qy = wx * s + wy * c
  in
    st { wx = qx, wy = qy }

cos :: Int -> Int
cos d = case d of
  (-270) -> 0
  (-180) -> -1
  (-90) -> 0
  90 -> 0
  180 -> -1
  270 -> 0
  _ -> unsafeThrow "This is a lazy cosine function"

sin :: Int -> Int
sin d = case d of
  (-270) -> 1
  (-180) -> 0
  (-90) -> -1
  90 -> 1
  180 -> 0
  270 -> -1
  _ -> unsafeThrow "This is a lazy sine function"

part2 :: String -> Effect Unit
part2 input = do
  let
    instructions = parse input
    initialState = { x: 0, y: 0, wx: 10, wy: 1 }
    { x, y } = foldl step2 initialState instructions
    result = abs x + abs y
  log $ "Part 2 ==> " <> show result

