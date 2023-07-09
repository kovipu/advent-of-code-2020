module Year2020.Day14 where

import Prelude
import Control.Alternative (empty)
import Data.Array.NonEmpty (toArray)
import Data.Array (fromFoldable, last, init, concatMap, (:), (!!), length)
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
import Data.Maybe (Maybe(..), fromJust, fromMaybe)
import Data.Tuple (fst)
import Effect (Effect)
import Effect.Class.Console (log, logShow)
import Effect.Exception (throw)
import Effect.Exception.Unsafe (unsafeThrow)
import Partial.Unsafe (unsafePartial)
import Parsing (Parser, runParser)
import Parsing.Combinators ((<|>), many1Till)
import Parsing.Combinators.Array (many1, many)
import Parsing.String (takeN, string, char, satisfy)

example :: String
example =
  """mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X
mem[8] = 11
mem[7] = 101
mem[8] = 0
"""

example2 :: String
example2 =
  """mask = 000000000000000000000000000000X1001X
mem[42] = 100
mask = 00000000000000000000000000000000X0XX
mem[26] = 1
"""

example3 :: String
example3 =
  """mask = 1110X1110XXX101X0011010X110X10X0110X
mem[40257] = 51331021
mem[18433] = 464024066
mem[9993] = 463909
mask = 11X011010X110X101X011X1X010X10100001
mem[54152] = 692939
mem[31079] = 22525259
mem[33597] = 474240
mem[3881] = 919507
mem[24651] = 48975360
mem[14815] = 1554
mem[17731] = 1337580
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
      if b == 1 then acc + (two `pow` (fromInt (35 - idx)))
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
  let
    { memory } = runProgram instructions
    result = foldl (+) zer0 memory
  log $ "Part 1 ==> " <> toString result

--------------------------------------------------------------------------------

type State2 = { mask :: String, memory :: Map BigInt BigInt }

runProgram2 :: Array Instruction -> State2
runProgram2 = foldl step2 { mask: "", memory: Map.empty }

step2 :: State2 -> Instruction -> State2
step2 st@{ mask, memory } inst =
  case inst of
    Mask m ->
      st { mask = m }
    Mem addr val ->
      -- take each masked addr -> write val to it
      let
        addresses = unsafePartial $ maskAddr addr mask
        v = fromInt val
      in
        st { memory = foldl (\acc a -> Map.insert a v acc) memory addresses }

maskAddr :: Partial => Int -> String -> Array BigInt
maskAddr addr mask =
  -- convert addr to binary
  let
    addrBinary = toBinary addr
    maskArr = mask # toCharArray
    first = last maskArr # fromJust
    rest = init maskArr # fromJust
    initial = case first of
      'X' -> [ [ 0 ], [ 1 ] ]
      '1' -> [ [ 1 ] ]
      '0' -> [ [ addrBinary !! 0 # fromJust ] ]
  -- generate all possible addresses
  in
    foldrWithIndex
      ( \idx b acc ->
          case b of
            '0' ->
              -- add unchanged bit to each addr
              let
                bit = addrBinary !! (35 - idx) # fromMaybe 0
              in
                map (\e -> bit : e) acc
            '1' ->
              -- add 1 to each addr
              map (\e -> 1 : e) acc
            'X' ->
              -- add 0 & 1 doubling addresses
              concatMap (\e -> [ 0 : e, 1 : e ]) acc
      )
      initial
      rest
      # map toInt

toBinary :: Int -> Array Int
toBinary n = case n of
  0 -> [ 0 ]
  n | n `mod` 2 == 1 -> 1 : toBinary (n `div` 2)
  n | n `mod` 2 == 0 -> 0 : toBinary (n `div` 2)
  _ -> unsafeThrow "how?"

part2 :: String -> Effect Unit
part2 input = do
  instructions <- parse input
  let
    { memory } = runProgram2 instructions
    result = foldl (+) zer0 memory

  log $ "Part 2 ==> " <> toString result

