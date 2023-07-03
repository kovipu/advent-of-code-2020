module Year2020.Day11 where

import Prelude
import Data.Array (filter, mapWithIndex, (!!), concat)
import Data.Foldable (foldl)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Pair (Pair, (~))
import Data.Show.Generic (genericShow)
import Data.String (split)
import Data.String.CodeUnits (toCharArray)
import Data.String.Pattern (Pattern(..))
import Effect (Effect)
import Effect.Class.Console (log, logShow)
import Effect.Exception.Unsafe (unsafeThrow)

example :: String
example =
  """L.LL.LL.LL
LLLLLLL.LL
L.L.L..L..
LLLL.LL.LL
L.LL.LL.LL
L.LLLLL.LL
..L.L.....
LLLLLLLLLL
L.LLLLLL.L
L.LLLLL.LL
"""

data Tile = Floor | Empty | Occupied

derive instance eqTile :: Eq Tile
derive instance genericTile :: Generic Tile _

instance showTile :: Show Tile where
  show = genericShow

type Grid = Array (Array Tile)

parse :: String -> Grid
parse input =
  split (Pattern "\n") input
    # filter (_ /= "")
    # map parseRow

parseRow :: String -> Array Tile
parseRow row =
  map
    ( \t -> case t of
        '.' -> Floor
        'L' -> Empty
        _ -> unsafeThrow "data is ass"
    ) $ toCharArray row

neighborOffsets =
  [ -1 ~ -1
  , 0 ~ -1
  , 1 ~ -1
  , 1 ~ 0
  , 1 ~ 1
  , 0 ~ 1
  , -1 ~ 1
  , -1 ~ 0
  ]

--------------------------------------------------------------------------------
stabilize :: Grid -> Grid
stabilize grid =
  let
    newGrid = step grid
  in
    if newGrid == grid then newGrid
    else stabilize newGrid

step :: Grid -> Grid
step grid =
  mapWithIndex
    ( \y row -> mapWithIndex
        ( \x tile ->
            case tile of
              Floor -> Floor
              Empty ->
                if neighborCount grid x y == 0 then Occupied
                else Empty
              Occupied ->
                if neighborCount grid x y >= 4 then Empty
                else Occupied
        )
        row
    )
    grid

neighborCount :: Grid -> Int -> Int -> Int
neighborCount grid x y =
  foldl
    ( \acc (dx ~ dy) ->
        if isNeighbor grid (x + dx) (y + dy) then acc + 1
        else acc
    )
    0
    neighborOffsets

isNeighbor :: Grid -> Int -> Int -> Boolean
isNeighbor grid x y =
  case grid !! y of
    Nothing -> false
    Just row -> row !! x == Just Occupied

part1 :: String -> Effect Unit
part1 input = do
  let
    grid = parse input
    stabilized = stabilize grid
    result =
      foldl
        (\acc t -> if t == Occupied then acc + 1 else acc)
        0
        $ concat stabilized
  log $ "Part 1 ==> " <> show result

--------------------------------------------------------------------------------
stabilize2 :: Grid -> Grid
stabilize2 grid =
  let
    newGrid = step2 grid
  in
    if newGrid == grid then newGrid
    else stabilize2 newGrid

step2 :: Grid -> Grid
step2 grid =
  mapWithIndex
    ( \y row -> mapWithIndex
        ( \x tile ->
            case tile of
              Floor -> Floor
              Empty ->
                if neighborCount2 grid x y == 0 then Occupied
                else Empty
              Occupied ->
                if neighborCount2 grid x y >= 5 then Empty
                else Occupied
        )
        row
    )
    grid

neighborCount2 :: Grid -> Int -> Int -> Int
neighborCount2 grid x y =
  foldl
    ( \acc (dx ~ dy) ->
        if isNeighbor2 grid x y dx dy then acc + 1
        else acc
    )
    0
    neighborOffsets

isNeighbor2 :: Grid -> Int -> Int -> Int -> Int -> Boolean
isNeighbor2 grid x y dx dy =
  let
    newx = x + dx
    newy = y + dy
  in
    case grid !! newy of
      Nothing -> false
      Just row -> case row !! newx of
        -- out of bounds
        Just Occupied -> true
        -- recurse to see across floors
        Just Floor -> isNeighbor2 grid newx newy dx dy
        -- Empty or Nothing 
        _ -> false

part2 :: String -> Effect Unit
part2 input = do
  let
    grid = parse input
    stabilized = stabilize2 grid
    result =
      foldl
        (\acc t -> if t == Occupied then acc + 1 else acc)
        0
        $ concat stabilized
  log $ "Part 2 ==> " <> show result
