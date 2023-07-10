module Year2020.Day17 where

import Prelude

import AOC.Lib (iterate)
import Control.Alternative (guard)
import Data.Array (filter, (..), (:), length)
import Data.Foldable (foldl)
import Data.FoldableWithIndex (foldlWithIndex)
import Data.HashMap (HashMap)
import Data.HashMap as HashMap
import Data.Maybe (Maybe(..))
import Data.String (split, joinWith)
import Data.String.CodeUnits (fromCharArray)
import Data.String.CodeUnits (toCharArray)
import Data.String.Pattern (Pattern(..))
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Class.Console (log, logShow)
import Debug (spy)

example :: String
example =
  """.#.
..#
###
"""

--------------------------------------------------------------------------------
type Coords = { x :: Int, y :: Int, z :: Int }

-- manage dimension in a Map
-- all active cubes and neighboring inactive ones must be present.
type Dimension = HashMap Coords Boolean

parse :: String -> Dimension
parse input =
  split (Pattern "\n") input
    # filter (_ /= "")
    # foldlWithIndex
        ( \y dimension row ->
            foldlWithIndex
              ( \x dimension cube ->
                  -- if cube active -> add it and its neighbors to acc
                  if cube == '#' then
                    insertWithNeighbors dimension { x, y, z: 0 }
                  else dimension
              )
              dimension
              (toCharArray row)
        )
        HashMap.empty

insertWithNeighbors :: Dimension -> Coords -> Dimension
insertWithNeighbors dimension coords =
  let
    neighbors = getNeighbors coords
  in
    foldl
      ( \acc neighbor ->
          HashMap.insertWith
            (\prev _ -> prev)
            neighbor
            false
            acc
      )
      (HashMap.insert coords true dimension)
      neighbors

-- a cube has 26 neighbors
getNeighbors :: Coords -> Array Coords
getNeighbors { x, y, z } =
  -- z=-1
  [ { x: x - 1, y: y - 1, z: z - 1 }
  , { x: x, y: y - 1, z: z - 1 }
  , { x: x + 1, y: y - 1, z: z - 1 }
  , { x: x - 1, y: y, z: z - 1 }
  , { x: x, y: y, z: z - 1 }
  , { x: x + 1, y: y, z: z - 1 }
  , { x: x - 1, y: y + 1, z: z - 1 }
  , { x: x, y: y + 1, z: z - 1 }
  , { x: x + 1, y: y + 1, z: z - 1 }
  -- z=0
  , { x: x - 1, y: y - 1, z: z }
  , { x: x, y: y - 1, z: z }
  , { x: x + 1, y: y - 1, z: z }
  , { x: x - 1, y: y, z: z }
  , { x: x + 1, y: y, z: z }
  , { x: x - 1, y: y + 1, z: z }
  , { x: x, y: y + 1, z: z }
  , { x: x + 1, y: y + 1, z: z }
  -- z=+1
  , { x: x - 1, y: y - 1, z: z + 1 }
  , { x: x, y: y - 1, z: z + 1 }
  , { x: x + 1, y: y - 1, z: z + 1 }
  , { x: x - 1, y: y, z: z + 1 }
  , { x: x, y: y, z: z + 1 }
  , { x: x + 1, y: y, z: z + 1 }
  , { x: x - 1, y: y + 1, z: z + 1 }
  , { x: x, y: y + 1, z: z + 1 }
  , { x: x + 1, y: y + 1, z: z + 1 }
  ]

printDimension :: Dimension -> String
printDimension dimension =
  let
    printWidth = 8
    printRange = -printWidth .. printWidth
  in
    foldl
      ( \acc z ->
          acc <> "\nz=" <> show z
            <> foldl
              ( \acc y ->
                  acc <> foldl
                    ( \acc x ->
                        if HashMap.lookup { x, y, z } dimension == Just true then acc <> "#"
                        else acc <> "."
                    )
                    "\n"
                    printRange
              )
              ""
              printRange
            <> "\n"
      )
      ""
      printRange

countActiveNeighbors :: Dimension -> Coords -> Int
countActiveNeighbors dimension cube =
  foldl
    ( \acc neighbor ->
        if HashMap.lookup neighbor dimension == Just true then acc + 1
        else acc
    )
    0
    (getNeighbors cube)

step :: Dimension -> Dimension
step dimension =
  foldl
    ( \acc (cube /\ isActive) ->
        -- check each cube -> follow rules -> if active -> add neighbors
        let
          activeNeighbors = countActiveNeighbors dimension cube
        in
          if activeNeighbors == 3 then insertWithNeighbors acc cube
          else if activeNeighbors == 2 && isActive then insertWithNeighbors acc cube
          else acc
    )
    HashMap.empty
    (HashMap.toArrayBy Tuple dimension)

part1 :: String -> Effect Unit
part1 input = do
  let
    initial = parse input
    endState = iterate 6 step initial

    result = foldl
      (\acc c -> if c then acc + 1 else acc)
      0
      endState

  log $ "Part 1 ==> " <> show result

--------------------------------------------------------------------------------
type Coords4D = { x :: Int, y :: Int, z :: Int, w :: Int }

type Dimension4D = HashMap Coords4D Boolean

parse4D :: String -> Dimension4D
parse4D input =
  split (Pattern "\n") input
    # filter (_ /= "")
    # foldlWithIndex
        ( \y dimension row ->
            foldlWithIndex
              ( \x dimension cube ->
                  -- if cube active -> add it and its neighbors to acc
                  if cube == '#' then
                    insertWithNeighbors4D dimension { x, y, z: 0, w: 0 }
                  else dimension
              )
              dimension
              (toCharArray row)
        )
        HashMap.empty

insertWithNeighbors4D :: Dimension4D -> Coords4D -> Dimension4D
insertWithNeighbors4D dimension coords =
  let
    neighbors = getNeighbors4D coords
  in
    foldl
      ( \acc neighbor ->
          HashMap.insertWith
            (\prev _ -> prev)
            neighbor
            false
            acc
      )
      (HashMap.insert coords true dimension)
      neighbors

getNeighbors4D :: Coords4D -> Array Coords4D
getNeighbors4D { x, y, z, w } =
  map
    ( \n ->
        { x: x + n.x
        , y: y + n.y
        , z: z + n.z
        , w: w + n.w
        }
    )
    neighbors4D

neighbors4D :: Array Coords4D
neighbors4D = do
  let range = (-1) .. 1
  x <- range
  y <- range
  z <- range
  w <- range
  guard $ x /= 0 || y /= 0 || z /= 0 || w /= 0
  pure $ { x, y, z, w }

countActiveNeighbors4D :: Dimension4D -> Coords4D -> Int
countActiveNeighbors4D dimension cube =
  foldl
    ( \acc neighbor ->
        if HashMap.lookup neighbor dimension == Just true then acc + 1
        else acc
    )
    0
    (getNeighbors4D cube)

step4D :: Dimension4D -> Dimension4D
step4D dimension =
  foldl
    ( \acc (cube /\ isActive) ->
        -- check each cube -> follow rules -> if active -> add neighbors
        let
          activeNeighbors = countActiveNeighbors4D dimension cube
        in
          if activeNeighbors == 3 then insertWithNeighbors4D acc cube
          else if activeNeighbors == 2 && isActive then insertWithNeighbors4D acc cube
          else acc
    )
    HashMap.empty
    (HashMap.toArrayBy Tuple dimension)

part2 :: String -> Effect Unit
part2 input = do
  let
    initial = parse4D input

    state1 = step4D initial
    _ = spy "state1 done" true

    state2 = step4D state1
    _ = spy "state2 done" true

    state3 = step4D state2
    _ = spy "state3 done" true

    state4 = step4D state3
    _ = spy "state4 done" true

    state5 = step4D state4
    _ = spy "state5 done" true

    endState = step4D state5
    _ = spy "endState done" true

    result = HashMap.size $ HashMap.filter identity endState

  log $ "Part 2 ==> " <> show result

