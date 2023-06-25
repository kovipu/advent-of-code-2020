module Year2020.Day08 where

import Prelude
import Control.Alternative (empty)
import Control.Monad.ST (run, while)
import Control.Monad.ST.Ref (new, read, modify)
import Data.Array ((:), (!!), elem, length)
import Data.Array.NonEmpty (toArray)
import Data.CodePoint.Unicode (isDecDigit)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Int (fromString)
import Data.Show.Generic (genericShow)
import Data.String.CodePoints (codePointFromChar)
import Data.String.CodeUnits (fromCharArray)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Exception (throw)
import Effect.Class.Console (log, logShow)
import Parsing (Parser, runParser)
import Parsing.String (takeN, char, satisfy)
import Parsing.Combinators ((<|>))
import Parsing.Combinators.Array (many1, many)

example :: String
example =
  """nop +0
acc +1
jmp +4
acc +3
jmp -3
acc -99
acc +1
jmp -4
acc +6
"""

data Instruction
  = Nop
  | Acc Int
  | Jmp Int

derive instance genericInstruction :: Generic Instruction _

instance showInstruction :: Show Instruction where
  show = genericShow

parseInstruction :: Parser String Instruction
parseInstruction = do
  op <- takeN 3
  _ <- char ' '
  sign <- char '+' <|> char '-'
  val <- parseNumber
  _ <- char '\n'
  let
    value = if sign == '-' then -1 * val else val
  case op of
    "nop" -> pure Nop
    "acc" -> pure $ Acc value
    "jmp" -> pure $ Jmp value
    _ -> empty

parseMaybeInt :: Maybe Int -> Parser String Int
parseMaybeInt s = case s of
  Just n -> pure n
  Nothing -> empty

parseNumber :: Parser String Int
parseNumber =
  many1 (satisfy (codePointFromChar >>> isDecDigit))
    # map (toArray >>> fromCharArray >>> fromString)
    >>= parseMaybeInt

-- if infinite loop -> Left acc
-- if exit cleanly -> Right acc
runInstructions :: Array Instruction -> Either Int Int
runInstructions instructions =
  run do
    let
      end = length instructions
    ref <- new { pointer: 0, accumulator: 0, executed: [] }
    while
      ( read ref
          # map
              ( \{ pointer, executed } ->
                  pointer < end && (not $ elem pointer executed)
              )
      )
      ( modify
          ( \{ pointer, accumulator, executed } -> case instructions !! pointer of
              Just Nop ->
                { pointer: pointer + 1
                , accumulator
                , executed: pointer : executed
                }
              Just (Acc n) ->
                { pointer: pointer + 1
                , accumulator: accumulator + n
                , executed: pointer : executed
                }
              Just (Jmp n) ->
                { pointer: pointer + n
                , accumulator
                , executed: pointer : executed
                }
              Nothing -> { pointer, accumulator, executed }
          )
          ref
      )
    final <- read ref
    -- possible bug: we can jump out of bounds to end if unlucky
    if final.pointer == end then
      pure $ Right final.accumulator
    else
      pure $ Left final.accumulator

--------------------------------------------------------------------------------
part1 :: String -> Effect Unit
part1 input = do
  let
    instructions = case runParser input (many parseInstruction) of
      Right i -> i
      _ -> []
  result <- case runInstructions instructions of
    Left n -> pure n
    Right _ -> throw "no infinite loop"
  log $ "Part 1 ==> " <> show result

--------------------------------------------------------------------------------
-- find the jmp or nop which fixes the code.
-- change one -> run the code and see if it loops or not
part2 :: String -> Effect Unit
part2 input = do
  let
    instructions = case runParser input (many parseInstruction) of
      Right i -> i
      _ -> []

    result = "<TODO>"
  log $ "Part 2 ==> " <> result
