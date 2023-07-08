module Year2020.Day14 where

import Prelude
import Control.Alternative (empty)
import Data.Array.NonEmpty (toArray)
import Data.Array (fromFoldable)
import Data.BigInt (BigInt, fromInt, toString, pow, and, or)
import Data.CodePoint.Unicode (isAscii, isDecDigit)
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.FoldableWithIndex (foldrWithIndex)
import Data.Generic.Rep (class Generic)
import Data.Int (fromString)
import Data.Map (Map)
import Data.Map as Map
import Data.Show.Generic (genericShow)
import Data.String.CodePoints (codePointFromChar)
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Data.Maybe (Maybe(..))
import Data.Tuple (fst)
import Effect (Effect)
import Effect.Class.Console (log, logShow)
import Effect.Exception (throw)
import Parsing (Parser, runParser)
import Parsing.Combinators ((<|>), many1Till)
import Parsing.Combinators.Array (many1, many)
import Parsing.String (takeN, string, char, satisfy)
import Debug (spy)

example :: String
example =
  """mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X
mem[8] = 11
mem[7] = 101
mem[8] = 0
"""

data Instruction
  = Mask String
  | Mem Int Int

derive instance genericInstruction :: Generic Instruction _

instance showInstruction :: Show Instruction where
  show = genericShow

parse :: String -> Effect (Array Instruction)
parse input = case runParser input (many parseInst) of
  Right i -> pure i
  _ -> throw "Failed to parse"

parseInst :: Parser String Instruction
parseInst = do
  op <- takeN 4
  let memOp = op == "mem["
  addr <- if memOp then parseNumber else pure 0
  _ <- string "] = " <|> string " = "
  val <- parseString
  if memOp then case fromString val of
    Just n -> pure $ Mem addr n
    Nothing -> empty
  else pure $ Mask val

parseNumber :: Parser String Int
parseNumber =
  many1 (satisfy (codePointFromChar >>> isDecDigit))
    # map (toArray >>> fromCharArray >>> fromString)
    >>= parseMaybeInt

parseMaybeInt :: Maybe Int -> Parser String Int
parseMaybeInt s = case s of
  Just n -> pure n
  Nothing -> empty

parseString :: Parser String String
parseString =
  let
    p = satisfy (codePointFromChar >>> isAscii)
    end = char '\n'
  in
    many1Till p end
      # map (fromFoldable >>> fromCharArray)

--------------------------------------------------------------------------------

type State = { mask :: String, memory :: Map Int BigInt }

runProgram :: Array Instruction -> State
runProgram = foldl step { mask: "", memory: Map.empty }

step :: State -> Instruction -> State
step st@{ mask, memory } inst =
  case inst of
    Mask m ->
      st { mask = m }
    Mem addr val ->
      st { memory = Map.insert addr (bitmask val mask) memory }

bitmask :: Int -> String -> BigInt
bitmask val mask =
  let
    -- 0 and X -> and 0 and 1
    exces = toCharArray mask
      # map (\b -> if b == 'X' then 1 else 0)
      # toInt
    -- 1 -> or 1
    ones = toCharArray mask
      # map (\b -> if b == '1' then 1 else 0)
      # toInt
  in
     exces `and` (fromInt val) `or` ones

toInt :: Array Int -> BigInt
toInt = foldrWithIndex
  ( \idx b acc ->
      if b == 1
        then acc + (two `pow` (fromInt (35 - idx)))
        else acc
  )
  zer0

two :: BigInt
two = fromInt 2

zer0 :: BigInt
zer0 = fromInt 0

part1 :: String -> Effect Unit
part1 input = do
  instructions <- parse input
  let { memory } = runProgram instructions
      result = foldl (+) zer0 memory
  log $ "Part 1 ==> " <> toString result

--------------------------------------------------------------------------------

part2 :: String -> Effect Unit
part2 input = do
  let result = "<TODO>"
  log $ "Part 2 ==> " <> result

