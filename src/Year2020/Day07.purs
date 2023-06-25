module Year2020.Day07 where

import Prelude
import Effect (Effect)
import Effect.Class.Console (log, logShow)
import Control.Alternative (empty)
import Data.Array (nub, length)
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.Int (fromString)
import Data.String.CodePoints (codePointFromChar, singleton)
import Data.String.CodeUnits (fromCharArray)
import Data.CodePoint.Unicode (isAlpha, isDecDigit)
import Data.Tuple (fst)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Parsing (Parser, runParser)
import Parsing.String (satisfy, char, anyChar, string)
import Parsing.Combinators ((<|>), option, optional)
import Parsing.Combinators.Array (many, manyTill_)

test :: String
test =
  """light red bags contain 1 bright white bag, 2 muted yellow bags.
dark orange bags contain 3 bright white bags, 4 muted yellow bags.
bright white bags contain 1 shiny gold bag.
muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
dark olive bags contain 3 faded blue bags, 4 dotted black bags.
vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
faded blue bags contain no other bags.
dotted black bags contain no other bags.
"""

type Rule
  = { container :: String
    , content :: Array Content
    }

type Content
  = { color :: String
    , number :: Int
    }

parseRule :: Parser String Rule
parseRule = do
  color <- parseColor
  _ <- string "bags contain "
  content <- many parseContent
  _ <- optional $ string "no other bags."
  _ <- char '\n'
  pure { container: color, content: content }

parseColor :: Parser String String
parseColor = do
  colorPrefix <- parseString
  color <- parseString
  pure $ colorPrefix <> " " <> color

parseString :: Parser String String
parseString =
  let
    p = satisfy (codePointFromChar >>> isAlpha)

    end = char ' '
  in
    manyTill_ p end
      # map (fst >>> fromCharArray)

parseContent :: Parser String Content
parseContent = do
  num <- anyChar
  let
    number = codePointFromChar num # singleton # fromString
  _ <- char ' '
  color <- parseColor
  _ <- string "bags" <|> string "bag"
  _ <- string ", " <|> string "."
  case number of
    Just n -> pure { color: color, number: n }
    Nothing -> empty

--------------------------------------------------------------------------------
type BagMap
  = Map String (Array String)

getBagMap :: Array Rule -> BagMap
getBagMap = foldl addToBagMap Map.empty

addToBagMap :: BagMap -> Rule -> BagMap
addToBagMap b r =
  foldl
    (\acc c -> Map.insertWith (<>) c.color [ r.container ] acc)
    b
    r.content

findOuterBags :: String -> BagMap -> Array String
findOuterBags bag bagmap =
  let
    directly =
      Map.lookup bag bagmap
        # fromMaybe []
  in
    foldl (\acc d -> acc <> findOuterBags d bagmap) directly directly
      # nub

part1 :: String -> Effect Unit
part1 input = do
  let
    rules = case runParser input (many parseRule) of
      Right r -> r
      Left _ -> []

    bagmap = getBagMap rules

    result = length $ findOuterBags "shiny gold" bagmap
  log $ "Part 1 ==> " <> show result

--------------------------------------------------------------------------------
type RuleMap
  = Map String (Array Content)

getRuleMap :: Array Rule -> RuleMap
getRuleMap rules =
  foldl
    (\acc { container, content } -> Map.insert container content acc)
    Map.empty
    rules

getBagNumber :: String -> RuleMap -> Int
getBagNumber bag rulemap =
  Map.lookup bag rulemap
    # fromMaybe []
    # foldl (\acc { number, color } -> acc + (number * getBagNumber color rulemap)) 1

part2 :: String -> Effect Unit
part2 input = do
  let
    rules = case runParser input (many parseRule) of
      Right r -> r
      Left _ -> []

    rulemap = getRuleMap rules

    result = getBagNumber "shiny gold" rulemap
  log $ "Part 2 ==> " <> show (result - 1) -- off by one :D
