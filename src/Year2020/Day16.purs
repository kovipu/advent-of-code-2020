module Year2020.Day16 where

import Prelude
import Control.Alternative (empty)
import Control.Apply (lift5)
import Data.Array (any, filter, all, transpose, find, (:), unsafeIndex, mapWithIndex, length, null)
import Data.BigInt (BigInt, fromInt, toString)
import Data.CodePoint.Unicode (isDecDigit)
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.FoldableWithIndex (foldrWithIndex)
import Data.String.Utils (startsWith)
import Data.Maybe (Maybe(..), fromJust)
import Data.Map (Map)
import Data.Map as Map
import Data.Int (fromString)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Class.Console (log, logShow)
import Effect.Exception (throw)
import Partial.Unsafe (unsafePartial)
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

example2 :: String
example2 =
  """class: 0-1 or 4-19
row: 0-5 or 8-19
seat: 0-13 or 16-19

your ticket:
11,12,13

nearby tickets:
3,9,18
15,1,5
5,14,9
"""

type Notes =
  { rules :: Array Rule
  , ticket :: Array Int
  , nearbyTickets :: Array Ticket
  }

type Ticket = Array Int

data Rule = Rule String Int Int Int Int

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
  case lift5 Rule (Just rule) (fromString a) (fromString b) (fromString c) (fromString d) of
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
testRule n (Rule _ a b c d) =
  (a <= n && n <= b) || (c <= n && n <= d)

part1 :: String -> Effect Unit
part1 input = do
  notes <- parse input
  let
    result = getScanningErrorRate notes
  log $ "Part 1 ==> " <> show result

--------------------------------------------------------------------------------

filterValid :: Array Rule -> Array Ticket -> Array Ticket
filterValid rules tickets =
  filter
    ( \ticket ->
        -- possibility that not all rules are matched to just one column?
        -- that would mean no Nothings in column names
        all
          (\n -> any (\rule -> testRule n rule) rules)
          ticket
    )
    tickets

-- which rule for each column?
getColumnNames :: Array Rule -> Array Ticket -> Map String Int
getColumnNames rules tickets =
  -- a rule can match for multiple columns -> find column which only matches for a rule
  let
    columns = transpose tickets
      # mapWithIndex Tuple
  in
    unsafePartial $ getRuleIdxRecursive Map.empty columns rules

getRuleIdxRecursive :: Partial => Map String Int -> Array (Tuple Int (Array Int)) -> Array Rule -> Map String Int
getRuleIdxRecursive names cols rules =
  -- get 1st rule, that matches only one col -> add to Map
  let
    -- find rule that only matches a column.
    rule@(Rule name _ _ _ _) =
      find
        ( \r ->
            filter
              (\(_ /\ c) -> all (\n -> testRule n r) c)
              cols
              # length
              # (_ == 1)
        )
        rules
        # fromJust
    -- another find for that column...
    idx /\ _ =
      find
        (\(_ /\ c) -> all (\n -> testRule n rule) c)
        cols
        # fromJust

    newNames = Map.insert name idx names

    rulesRest = filter (\(Rule r _ _ _ _) -> r /= name) rules
    colsRest = filter (\(i /\ _) -> i /= idx) cols

  -- recurse until all columns are get.
  in
    if null rulesRest then newNames
    else getRuleIdxRecursive newNames colsRest rulesRest

part2 :: String -> Effect Unit
part2 input = do
  { rules, ticket, nearbyTickets } <- parse input
  let
    validTickets = ticket : (filterValid rules nearbyTickets)
    columnNames = getColumnNames rules validTickets

    departures = Map.filterWithKey
      (\k _ -> startsWith "departure" k)
      columnNames

    result = foldl
      (\acc idx -> acc * (fromInt (unsafePartial $ unsafeIndex ticket idx)))
      (fromInt 1)
      departures

  log $ "Part 2 ==> " <> toString result

