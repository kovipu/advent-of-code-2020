module Year2020.Day17 where

import Prelude

import AOC.Lib (iterate)
import Data.Array (filter, (..))
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
  let
    neighbors = getNeighbors cube
  in
    foldl
      ( \acc neighbor ->
          if HashMap.lookup neighbor dimension == Just true then acc + 1
          else acc
      )
      0
      neighbors

--------------------------------------------------------------------------------
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

part2 :: String -> Effect Unit
part2 input = do
  let result = "<TODO>"
  log $ "Part 2 ==> " <> result

