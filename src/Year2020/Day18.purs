module Year2020.Day18 where

import Prelude

import Control.Alternative (empty)
import Control.Lazy (fix)
import Data.Array (length, reverse)
import Data.BigInt (BigInt, fromInt, toString)
import Data.Foldable (foldl, sum)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Int (fromString)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Data.String (split, joinWith, replaceAll, Replacement(..))
import Data.String.CodeUnits (toCharArray, fromCharArray)
import Data.String.Pattern (Pattern(..))
import Effect (Effect)
import Effect.Class.Console (log, logShow)
import Effect.Exception (throw)
import Parsing (Parser, runParser)
import Parsing.Combinators ((<|>), option, optional, between, chainl1, try)
import Parsing.Combinators.Array (many)
import Parsing.String (anyTill, string, char, anyChar)
import Parsing.String.Basic (intDecimal)
import Debug (spy)

example :: String
example =
  """1 + 2 * 3 + 4 * 5 + 6
"""

example2 :: String
example2 =
  """1 + (2 * 3) + (4 * (5 + 6))
"""

data Expr
  = Add Expr Expr
  | Mul Expr Expr
  | Lit Int

instance Show Expr where
  show (Add e1 e2) = "(" <> show e1 <> " + " <> show e2 <> ")"
  show (Mul e1 e2) = "(" <> show e1 <> " * " <> show e2 <> ")"
  show (Lit l) = show l

-- order of operations was reversed -> smooth brain solution
parse :: String -> Effect (Array Expr)
parse input =
  let
    reversed = split (Pattern "\n") input
      # map
          ( toCharArray
              >>> reverse
              >>>
                ( map \c -> case c of
                    ')' -> '('
                    '(' -> ')'
                    other -> other
                )
              >>> fromCharArray
          )
      # joinWith "\n"
  in
    case runParser reversed (many expr) of
      Right r -> pure r
      Left _ -> throw "parsing failed"

data Op = Plus | Mult

expr :: Parser String Expr
expr =
  let
    start :: Parser String Expr
    start = group <|> lit

    group :: Parser String Expr
    group = do
      _ <- char '('
      e <- expr
      _ <- char ')'
      pure e

    end :: Parser String (Maybe (Tuple Op Expr))
    end =
      let
        getEnd = do
          _ <- char ' '
          op <- char '+' <|> char '*'
          _ <- char ' '
          e <- expr
          case op of
            '+' -> pure $ Just $ Plus /\ e
            '*' -> pure $ Just $ Mult /\ e
            _ -> empty
      in
        getEnd <|> pure Nothing
  in
    do
      e1 <- start
      mE2 <- end
      _ <- optional $ char '\n'
      case mE2 of
        Nothing -> pure e1
        Just (Plus /\ e2) -> pure $ Add e1 e2
        Just (Mult /\ e2) -> pure $ Mul e1 e2

lit :: Parser String Expr
lit = Lit <$> intDecimal

--------------------------------------------------------------------------------

evaluate :: Expr -> BigInt
evaluate (Add e1 e2) = evaluate e1 + evaluate e2
evaluate (Mul e1 e2) = evaluate e1 * evaluate e2
evaluate (Lit l) = (fromInt l)

part1 :: String -> Effect Unit
part1 input = do
  expressions <- parse input
  let
    result :: BigInt
    result = foldl
      (\acc e -> acc + (evaluate e))
      (fromInt 0)
      expressions
  log $ "Part 1 ==> " <> toString result

--------------------------------------------------------------------------------
parse2 :: String -> Effect (Array Expr)
parse2 input =
  case runParser input (many (expr2 <* char '\n')) of
    Right r -> pure r
    Left _ -> throw "parsing failed"

expr2 :: Parser String Expr
expr2 = fix \p ->
  let
    atom :: Parser String Expr
    atom = lit <|> between (char '(') (char ')') p

    plus :: Parser String Expr
    plus = chainl1 atom (try (string " + ") $> Add)
  in
    chainl1 plus (try (string " * ") $> Mul)

part2 :: String -> Effect Unit
part2 input = do
  expressions <- parse2 input
  let
    result :: BigInt
    result = map evaluate expressions # sum
  log $ "Part 2 ==> " <> toString result

