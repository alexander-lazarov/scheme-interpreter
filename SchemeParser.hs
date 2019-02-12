module SchemeParser
   ( nullLiteral
   , boolLiteral
   , intLiteral
   , stringLiteral
   , arithmeticOp
   , identifier
   , ifStatement
   , functionCall
   , expression
   )
where

import Parser
import SchemeGrammar
import Data.Char (ord)
import Data.List (foldl1')

whitespace :: Parser Char
whitespace = char ' '
         <|> char '\n'
         <|> char '\r'

optionalWhitespace :: Parser String
optionalWhitespace = many whitespace

mandatoryWhitespace :: Parser String
mandatoryWhitespace = some whitespace

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

arithmeticOp :: Parser Expression
arithmeticOp = do
  c <- char '+' <|> char '-' <|> char '*' <|> char '/'
  return $ ArithmeticOp c

identifier :: Parser Expression
identifier = do
    x <- letter
    xs <- many (alnum <|> char '-')
    result $ Identifier (x:xs)

ifStatement :: Parser Expression
ifStatement = do
  optionalWhitespace
  char '('
  optionalWhitespace

  string "if"
  mandatoryWhitespace

  p  <- expression
  mandatoryWhitespace

  t <- expression

  mandatoryWhitespace
  f  <- expression

  optionalWhitespace
  char ')'
  optionalWhitespace
  result $ IfStatement p t f

functionCall :: Parser Expression
functionCall = do
    optionalWhitespace
    char '('
    optionalWhitespace
    e  <- expression
    es <- many $ do
      mandatoryWhitespace
      ee <- expression
      result $ ee
    optionalWhitespace
    char ')'
    optionalWhitespace
    result $ FunctionCall e es

expression :: Parser Expression
expression =
             nullLiteral
         <|> boolLiteral
         <|> intLiteral
         <|> stringLiteral
         <|> arithmeticOp
         <|> identifier
         <|> functionCall
