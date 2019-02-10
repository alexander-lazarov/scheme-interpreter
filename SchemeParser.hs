module SchemeParser
   ( nullLiteral
   , BoolLiteral
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

BoolLiteral :: Parser Expression
BoolLiteral = true <|> false

identifier :: Parser Expression
identifier = do
    x <- letter
    xs <- many alnum
    result $ Identifier (x:xs)

-- identifiersList :: Parser [Expression]
-- identifiersList = do
--     i <- identifier
--     is <- many $ do
--        char ' '
--        ii <- identifier
--        result $ ii
--     result $ i:is

functionCall :: Parser Expression
functionCall = do
    char '('
    e  <- expression
    es <- many $ do
      char ' '
      ee <- expression
      result $ ee
    char ')'
    result $ FunctionCall (e:es)

expression :: Parser Expression
expression = identifier <|> functionCall