module Year2020.Day04 where

import Prelude
import Data.Array (concatMap, all)
import Data.Foldable (foldl)
import Data.Map (Map, insert, empty, member)
import Data.String (split)
import Data.String.Pattern (Pattern(..))
import Effect (Effect)
import Effect.Class.Console (log, logShow)

test :: String
test =
  """ecl:gry pid:860033327 eyr:2020 hcl:#fffffd
byr:1937 iyr:2017 cid:147 hgt:183cm

iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884
hcl:#cfa07d byr:1929

hcl:#ae17e1 iyr:2013
eyr:2024
ecl:brn pid:760753108 byr:1931
hgt:179cm

hcl:#cfa07d eyr:2025 pid:166559648
iyr:2011 ecl:brn hgt:59in"""

type Passport
  = Map String String

parse :: String -> Array Passport
parse input =
  split (Pattern "\n\n") input
    # map parsePassport

parsePassport :: String -> Passport
parsePassport passport =
  split (Pattern "\n") passport
    # concatMap (\s -> split (Pattern " ") s)
    # foldl
        ( \acc kv -> case split (Pattern ":") kv of
            [ k, v ] -> insert k v acc
            _ -> acc
        )
        empty

--------------------------------------------------------------------------------
isValid :: Passport -> Boolean
isValid passport =
  let
    requiredFields = [ "byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid" ]
  in
    all (\k -> member k passport) requiredFields

part1 :: String -> Effect Unit
part1 input = do
  let
    passports = parse input

    result = foldl (\acc p -> if isValid p then acc + 1 else acc) 0 passports
  log $ "Part 1 ==> " <> show result

--------------------------------------------------------------------------------
part2 :: String -> Effect Unit
part2 input = do
  let
    result = "<TODO>"
  log $ "Part 2 ==> " <> result
