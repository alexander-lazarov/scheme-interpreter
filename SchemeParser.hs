module SchemeParser
   ( nullLiteral
   , boolLiteral
   , intLiteral
   , stringLiteral
   , identifier
   , functionCall
   , expression
   )
where

import Parser
import SchemeGrammar
import Data.Char (ord)
import Data.List (foldl1')

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

digitParser :: Parser Integer
digitParser = fmap (fromIntegral . (subtract (ord '0')) . ord) $ digit

positiveInt' :: [Integer] -> Integer
positiveInt' = foldl1' (\r x -> r * 10 + x)

positiveInt :: Parser Expression
positiveInt = do
    ns <- some digitParser
    return $ IntLiteral $ positiveInt' ns

negativeInt = do
    char '-'
    ns <- some digitParser
    return $ IntLiteral $ -(positiveInt' ns)

intLiteral :: Parser Expression
intLiteral = positiveInt <|> negativeInt

stringLiteral :: Parser Expression
stringLiteral = do
    char '"'
    str <- many $ sat $ \c -> c /= '"'
    char '"'
    return $ StringLiteral str

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
expression =
             nullLiteral
         <|> boolLiteral
         <|> intLiteral
         <|> stringLiteral
         <|> identifier
         <|> functionCall