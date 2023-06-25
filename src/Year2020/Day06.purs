module Year2020.Day06 where

import Prelude
import Data.Array (nub, length, filter, all, elem, head)
import Data.Foldable (foldl)
import Data.String (split, replaceAll)
import Data.String.CodeUnits (toCharArray)
import Data.String.Pattern (Pattern(..), Replacement(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class.Console (log, logShow)

test :: String
test =
  """abc

a
b
c

ab
ac

a
a
a
a

b"""

--------------------------------------------------------------------------------
parseGroup :: String -> Int
parseGroup str =
  replaceAll (Pattern "\n") (Replacement "") str
    # toCharArray
    # nub
    # length

part1 :: String -> Effect Unit
part1 input = do
  let
    result =
      split (Pattern "\n\n") input
        # foldl (\acc g -> acc + parseGroup g) 0
  log $ "Part 1 ==> " <> show result

--------------------------------------------------------------------------------
parseGroup2 :: String -> Int
parseGroup2 str =
  let
    persons =
      split (Pattern "\n") str
        # filter (_ /= "")
        # map toCharArray

    firstPerson = head persons
  in
    case firstPerson of
      -- first person answers something -> check everyone answered it as well
      Just fp ->
        filter (\q -> all (\arr -> elem q arr) persons) fp
          # length
      Nothing -> 0

part2 :: String -> Effect Unit
part2 input = do
  let
    result =
      split (Pattern "\n\n") input
        # foldl (\acc g -> acc + parseGroup2 g) 0
  log $ "Part 2 ==> " <> show result
