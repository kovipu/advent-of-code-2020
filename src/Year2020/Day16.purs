module Year2020.Day16 where

import Prelude
import Control.Alternative (empty)
import Control.Apply (lift4)
import Data.Array (any)
import Data.CodePoint.Unicode (isDecDigit)
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.Maybe (Maybe(..))
import Data.Int (fromString)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Class.Console (log, logShow)
import Effect.Exception (throw)
import Parsing (Parser, runParser)
import Parsing.Combinators (optional)
import Parsing.Combinators.Array (many)
import Parsing.String (string, anyTill, char)
import Parsing.String.Basic (takeWhile1)
import Debug (spy)

example :: String
example =
  """class: 1-3 or 5-7
row: 6-11 or 33-44
seat: 13-40 or 45-50

your ticket:
7,1,14

nearby tickets:
7,3,47
40,4,50
55,2,20
38,6,12
"""

type Notes =
  { rules :: Array Rule
  , ticket :: Array Int
  , nearbyTickets :: Array Ticket
  }

type Ticket = Array Int

data Rule = Rule Int Int Int Int

derive instance genericRule :: Generic Rule _

instance showRule :: Show Rule where
  show = genericShow

parse :: String -> Effect Notes
parse input = case runParser input parseNotes of
  Right n -> pure n
  Left _ -> throw "failed to parse"

parseNotes :: Parser String Notes
parseNotes = do
  rules <- many parseRule
  _ <- string "\nyour ticket:\n"
  ticket <- parseTicket
  _ <- string "\nnearby tickets:\n"
  nearbyTickets <- many parseTicket
  pure { rules, ticket, nearbyTickets }

parseRule :: Parser String Rule
parseRule = do
  rule /\ _ <- anyTill $ string ": "
  a /\ _ <- anyTill $ char '-'
  b /\ _ <- anyTill $ string " or "
  c /\ _ <- anyTill $ char '-'
  d /\ _ <- anyTill $ char '\n'
  case lift4 Rule (fromString a) (fromString b) (fromString c) (fromString d) of
    Just r -> pure r
    Nothing -> empty

parseTicket :: Parser String Ticket
parseTicket = do
  ticket <- many
    ( do
        n <- parseInt
        _ <- optional $ char ','
        pure n
    )
  _ <- char '\n'
  pure $ ticket

parseInt :: Parser String Int
parseInt = do
  int <- takeWhile1 isDecDigit
  case fromString int of
    Just n -> pure n
    Nothing -> empty

--------------------------------------------------------------------------------

getScanningErrorRate :: Notes -> Int
getScanningErrorRate { rules, nearbyTickets } =
  foldl
    ( \acc ticket ->
        acc + (validateTicket ticket rules)
    )
    0
    nearbyTickets

validateTicket :: Ticket -> Array Rule -> Int
validateTicket ticket rules =
  foldl
    ( \acc n ->
        -- does n pass any of the rules?
        if any (\rule -> testRule n rule) rules then acc else acc + n
    )
    0
    ticket

testRule :: Int -> Rule -> Boolean
testRule n (Rule a b c d) =
  (a <= n && n <= b) || (c <= n && n <= d)

part1 :: String -> Effect Unit
part1 input = do
  notes <- parse input
  let
    result = getScanningErrorRate notes
  log $ "Part 1 ==> " <> show result

--------------------------------------------------------------------------------

part2 :: String -> Effect Unit
part2 input = do
  let result = "<TODO>"
  log $ "Part 2 ==> " <> result

