module Year2020.Day08 where

import Prelude
import Control.Alternative (empty)
import Control.Monad.ST (run, while)
import Control.Monad.ST.Ref (new, read, modify)
import Data.Array ((:), (!!), elem, length, updateAt)
import Data.Array.NonEmpty (toArray)
import Data.CodePoint.Unicode (isDecDigit)
import Data.Either (Either(..), isRight)
import Data.FoldableWithIndex (findWithIndex)
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
  = Nop Int
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
    "nop" -> pure $ Nop value
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
              Just (Nop _) ->
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
findFix :: Array Instruction -> Maybe { index :: Int, value :: Instruction }
findFix instructions =
  findWithIndex
    ( \idx inst ->
        -- change instruction at idx and see if it exits cleanly
        let
          updated = changeInst idx inst instructions
        in
          case updated of
            Just u -> isRight $ runInstructions u
            Nothing -> false
    )
    instructions

changeInst :: Int -> Instruction -> Array Instruction -> Maybe (Array Instruction)
changeInst idx inst instructions = case inst of
  Acc n -> Nothing
  Nop n -> updateAt idx (Jmp n) instructions
  Jmp n -> updateAt idx (Nop n) instructions

part2 :: String -> Effect Unit
part2 input = do
  instructions <- case runParser input (many parseInstruction) of
    Right i -> pure i
    Left _ -> throw "parsing failed"
  { index, value } <- case findFix instructions of
    Just n -> pure n
    Nothing -> throw "No way to fix"
  fixed <- case changeInst index value instructions of
    Just f -> pure f
    Nothing -> throw "you f'd up"
  final <- case runInstructions fixed of
    Right n -> pure n
    Left _ -> throw "should exit cleanly"
  log $ "Part 2 ==> " <> show final
