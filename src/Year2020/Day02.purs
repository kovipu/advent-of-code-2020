module Year2020.Day02 where

import Prelude
import Data.Foldable (foldl)
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Class.Console (log, logShow)
import Control.Alternative (empty)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Data.String.Unsafe (charAt)
import Data.String.CodePoints (codePointFromChar)
import Data.CodePoint.Unicode (isDecDigit, isAlpha)
import Parsing (Parser, runParser)
import Parsing.String (char, anyChar, string, satisfy)
import Parsing.Combinators.Array (manyTill_, many1, many)
import Data.Array.NonEmpty (toArray)
import Data.Int (fromString)
import Data.Int.Bits ((.^.))
import Data.Tuple (fst)

test :: String
test =
  """1-3 a: abcde
1-3 b: cdefg
2-9 c: ccccccccc
"""

type PasswordRow =
  { min :: Int
  , max :: Int
  , char :: Char
  , password :: String
  }

parseMaybeInt :: Maybe Int -> Parser String Int
parseMaybeInt s = case s of
  Just n -> pure n
  Nothing -> empty

parseNumber :: Parser String Int
parseNumber =
  many1 (satisfy (codePointFromChar >>> isDecDigit))
    # map (toArray >>> fromCharArray >>> fromString)
    >>= parseMaybeInt

parseString :: Parser String String -- parsii newlineen asti
parseString =
  let
    p = satisfy (\c -> isAlpha (codePointFromChar c))

    end = char '\n'
  in
    manyTill_ p end
      # map (fst >>> fromCharArray)

parseLine :: Parser String PasswordRow
parseLine = do
  a <- parseNumber
  _ <- char '-'
  b <- parseNumber
  _ <- char ' '
  c <- anyChar
  _ <- string ": "
  d <- parseString
  pure
    $
      { min: a
      , max: b
      , char: c
      , password: d
      }

--------------------------------------------------------------------------------
part1 :: String -> Effect Unit
part1 input = do
  let
    parsed = runParser input (many parseLine)

    policies = case parsed of
      Right p -> p
      _ -> []

    numValids = foldl (\acc p -> if isValid p then acc + 1 else acc) 0 policies
  log $ "Part 1 ==> " <> show numValids

isValid :: PasswordRow -> Boolean
isValid { min, max, char, password } =
  let
    occurences = countOccurences password char
  in
    min <= occurences && occurences <= max

countOccurences :: String -> Char -> Int
countOccurences str c =
  toCharArray str
    # foldl (\curr n -> if n == c then curr + 1 else curr) 0

--------------------------------------------------------------------------------
part2 :: String -> Effect Unit
part2 input = do
  let
    parsed = runParser input (many parseLine)

    policies = case parsed of
      Right p -> p
      _ -> []

    numValids = foldl (\acc p -> if isValid2 p then acc + 1 else acc) 0 policies
  log $ "Part 2 ==> " <> show numValids

isValid2 :: PasswordRow -> Boolean
isValid2 { min, max, char, password } =
  let
    a = charAt (min - 1) password

    b = charAt (max - 1) password

    ac = a == char

    bc = b == char
  in
    (ac && not bc) || (not ac && bc)
