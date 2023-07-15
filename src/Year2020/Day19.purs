module Year2020.Day19 where

import Prelude

import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.Generic.Rep (class Generic)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), isJust)
import Data.Show.Generic (genericShow)
import Data.String (uncons)
import Data.String.CodePoints (codePointFromChar)
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Class.Console (log)
import Effect.Exception (throw)
import Parsing (Parser, runParser)
import Parsing.Combinators ((<|>), try)
import Parsing.Combinators.Array (many)
import Parsing.String (string, anyTill)
import Parsing.String.Basic (intDecimal, space)

example :: String
example =
  """0: 1 2
1: \"a\"
2: 1 3 | 3 1
3: \"b\"

ababbb
bababa
abbbab
aaabbb
aaaabbb
"""

data Rule
  = A
  | B
  | Point Int
  | Pair Int Int
  | Or Int Int
  | Or2 Int Int Int Int

derive instance Generic Rule _
instance Show Rule where
  show x = genericShow x

type Rules = Map Int Rule

parse :: String -> Effect (Tuple Rules (Array String))
parse input =
  case runParser input parseInput of
    Right r -> pure r
    Left err -> throw $ show err

parseInput :: Parser String (Tuple Rules (Array String))
parseInput = do
  rules <- parseRules
  messages <- many parseMessage
  pure $ rules /\ messages

parseRules :: Parser String Rules
parseRules = do
  rules <- many parseRule
  pure $
    foldl
      (\acc (n /\ r) -> Map.insert n r acc)
      Map.empty
      rules

parseRule :: Parser String (Tuple Int Rule)
parseRule = do
  n <- intDecimal
  _ <- string ": "
  rule <- try aa <|> try bb <|> try or2 <|> try or <|> try pair <|> try point
  _ <- string "\n"
  pure $ n /\ rule

aa :: Parser String Rule
aa = string "\"a\"" $> A

bb :: Parser String Rule
bb = string "\"b\"" $> B

point :: Parser String Rule
point = intDecimal >>= (Point >>> pure)

or2 :: Parser String Rule
or2 = do
  a <- intDecimal
  _ <- space
  b <- intDecimal
  _ <- string " | "
  c <- intDecimal
  _ <- space
  d <- intDecimal
  pure $ Or2 a b c d

or :: Parser String Rule
or = do
  a <- intDecimal
  _ <- string " | "
  b <- intDecimal
  pure $ Or a b

pair :: Parser String Rule
pair = do
  a <- intDecimal
  _ <- string " "
  b <- intDecimal
  pure $ Pair a b

parseMessage :: Parser String String
parseMessage = do
  msg /\ _ <- anyTill (string "\n")
  pure msg

--------------------------------------------------------------------------------
matchRule :: Rules -> Int -> String -> Maybe String
matchRule rules ruleNo msg = do
  rule <- Map.lookup ruleNo rules
  case rule of
    Or2 a b c d ->
      -- do a & b match?
      let
        first =
          ( do
              rest <- matchRule rules a msg
              matchRule rules b rest
          )
      in
        if isJust first then first
        -- if not, do c & d match?
        else do
          rest <- matchRule rules c msg
          matchRule rules d rest
    Or a b ->
      let
        aRes = matchRule rules a msg
      in
        if isJust aRes then aRes
        else matchRule rules b msg
    Pair a b -> do
      rest <- matchRule rules a msg
      matchRule rules b rest
    Point a -> matchRule rules a msg
    A -> do
      { head, tail } <- uncons msg
      if head == codePointFromChar 'a' then pure tail else Nothing
    B -> do
      { head, tail } <- uncons msg
      if head == codePointFromChar 'b' then pure tail else Nothing

part1 :: String -> Effect Unit
part1 input = do
  rules /\ messages <- parse input
  let
    result = foldl
      ( \acc m ->
          if matchRule rules 0 m == Just "" then acc + 1
          else acc
      )
      0
      messages
  log $ "Part 1 ==> " <> show result

--------------------------------------------------------------------------------

part2 :: String -> Effect Unit
part2 input = do
  rules /\ nessages <- parse input
  let result = "<TODO>"
  log $ "Part 2 ==> " <> result

