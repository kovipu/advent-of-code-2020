module Year2020.Day04 where

import Prelude
import Control.Alternative (guard)
import Data.Array (concatMap, all, head, tail, elem)
import Data.CodePoint.Unicode (isDecDigit, isHexDigit)
import Data.String.CodeUnits (toCharArray)
import Data.String.CodePoints (codePointFromChar)
import Data.Foldable (foldl)
import Data.Int (fromString)
import Data.Map (Map, insert, empty, member, lookup)
import Data.Maybe (Maybe(..), isJust)
import Data.String (split, contains, takeWhile, length)
import Data.String.Pattern (Pattern(..))
import Effect (Effect)
import Effect.Class.Console (log)

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

type Passport = Map String String

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
    passports = parse input

    result =
      foldl
        ( \acc p -> case isValid2 p of
            Just true -> acc + 1
            _ -> acc
        )
        0
        passports
  log $ "Part 2 ==> " <> show result

isValid2 :: Passport -> Maybe Boolean
isValid2 p = do
  -- Birth Year
  byr <- lookup "byr" p >>= fromString
  guard $ 1920 <= byr && byr <= 2002
  -- Issue Year
  iyr <- lookup "iyr" p >>= fromString
  guard $ 2010 <= iyr && iyr <= 2020
  -- Expiration Year
  eyr <- lookup "eyr" p >>= fromString
  guard $ 2020 <= eyr && eyr <= 2030
  -- Height
  hgt <- lookup "hgt" p
  guard $ validateHeight hgt
  -- Hair color
  hcl <- lookup "hcl" p
  guard $ validateHairColor hcl
  -- Eye color
  ecl <- lookup "ecl" p
  guard $ elem ecl [ "amb", "blu", "brn", "gry", "grn", "hzl", "oth" ]
  -- Passport ID
  pid <- lookup "pid" p
  guard $ validatePid pid
  -- if all guards pass -> valid
  pure true

validateHeight :: String -> Boolean
validateHeight hgt =
  let
    height = fromString $ takeWhile isDecDigit hgt

    cm = contains (Pattern "cm") hgt
  in
    case height of
      Just h ->
        if cm then
          150 <= h && h <= 193
        else
          59 <= h && h <= 76
      _ -> false

validateHairColor :: String -> Boolean
validateHairColor hcl =
  let
    chars = toCharArray hcl

    prefix = head chars

    numbers = tail chars
  in
    case prefix, numbers of
      Just p, Just arr -> p == '#' && all (codePointFromChar >>> isHexDigit) arr
      _, _ -> false

validatePid :: String -> Boolean
validatePid pid = length pid == 9 && isJust (fromString pid)
