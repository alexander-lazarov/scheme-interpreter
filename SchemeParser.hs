module SchemeParser
   ( nullLiteral
   , boolLiteral
   , identifier
   , functionCall
   , expression
   )
where

import Parser
import SchemeGrammar

nullLiteral :: Parser Expression
nullLiteral = do
  string "null"
  return NullLiteral

true :: Parser Expression
true = do
  string "#t"
  return $ BoolLiteral True

false :: Parser Expression
false = do
  string "#f"
  return $ BoolLiteral False

boolLiteral :: Parser Expression
boolLiteral = true <|> false

identifier :: Parser Expression
identifier = do
    x <- letter
    xs <- many (alnum <|> char '-')
    result $ Identifier (x:xs)

functionCall :: Parser Expression
functionCall = do
    char '('
    e  <- expression
    es <- many $ do
      char ' '
      ee <- expression
      result $ ee
    char ')'
    result $ FunctionCall e es

expression :: Parser Expression
expression = identifier <|> functionCall