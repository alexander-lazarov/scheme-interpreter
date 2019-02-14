module SchemeParser
   ( nullLiteral
   , boolLiteral
   , intLiteral
   , stringLiteral
   , arithmeticOp
   , compOp
   , identifier
   , ifStatement
   , defineStatement
   , functionCall
   , expression
   , program
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
  _ <- string "null"
  return NullLiteral

true :: Parser Expression
true = do
  _ <- string "#t"
  return $ BoolLiteral True

false :: Parser Expression
false = do
  _ <- string "#f"
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

negativeInt :: Parser Expression
negativeInt = do
    _ <- char '-'
    ns <- some digitParser
    return $ IntLiteral $ -(positiveInt' ns)

intLiteral :: Parser Expression
intLiteral = positiveInt <|> negativeInt

stringLiteral :: Parser Expression
stringLiteral = do
    _ <- char '"'
    str <- many $ sat $ \c -> c /= '"'
    _ <- char '"'
    return $ StringLiteral str

arithmeticOp :: Parser Expression
arithmeticOp = do
  c <- char '+' <|> char '-' <|> char '*' <|> char '/'
  return $ ArithmeticOp c

compOp :: Parser Expression
compOp = do
  c <- char '=' <|> char '<' <|> char '>'
  return $ CompOp c

identifierString :: Parser String
identifierString = do
  x <- letter
  xs <- many (alnum <|> char '-')
  result $ x:xs

identifier :: Parser Expression
identifier = do
    x <- letter
    xs <- many (alnum <|> char '-')
    result $ Identifier (x:xs)

ifStatement :: Parser Expression
ifStatement = do
  _ <- optionalWhitespace
  _ <- char '('
  _ <- optionalWhitespace

  _ <- string "if"
  _ <- mandatoryWhitespace

  p <- expression
  _ <- mandatoryWhitespace

  t <- expression

  _ <- mandatoryWhitespace
  f <- expression

  _ <- optionalWhitespace
  _ <- char ')'
  _ <- optionalWhitespace
  result $ IfStatement p t f

defineStatement :: Parser Expression
defineStatement = do
  _ <- optionalWhitespace
  _ <- char '('
  _ <- optionalWhitespace

  _ <- string "define"
  _ <- mandatoryWhitespace

  _ <- char '('
  _ <- optionalWhitespace
  funcName <- identifierString
  arguments <- many $ do
    _ <- mandatoryWhitespace
    identifierString

  _ <- optionalWhitespace
  _ <- char ')'
  _ <- mandatoryWhitespace
  body <- expression
  _ <- optionalWhitespace
  _ <- char ')'
  _ <- optionalWhitespace
  result $ DefineStatement funcName arguments body

functionCall :: Parser Expression
functionCall = do
    _ <- optionalWhitespace
    _ <- char '('
    _ <- optionalWhitespace
    e  <- expression
    es <- many $ do
      _ <- mandatoryWhitespace
      ee <- expression
      result $ ee
    _ <- optionalWhitespace
    _ <- char ')'
    _ <- optionalWhitespace
    result $ FunctionCall e es

expression :: Parser Expression
expression =
             nullLiteral
         <|> boolLiteral
         <|> intLiteral
         <|> stringLiteral
         <|> arithmeticOp
         <|> compOp
         <|> identifier
         <|> ifStatement
         <|> defineStatement
         <|> functionCall

program :: Parser [Expression]
program = do
  es <- many $ do
    _ <- optionalWhitespace
    e <- expression
    _ <- optionalWhitespace
    return e
  endOfInput
  result $ es
