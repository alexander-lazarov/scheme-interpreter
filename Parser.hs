{-# LANGUAGE InstanceSigs #-}

module Parser
    ( Parser(..)
    , (<|>)
    , parse
    , result
    , empty
    , nom
    , sat
    , char
    , string
    , many
    , some
    , lowercase
    , uppercase
    , letter
    , digit
    , alnum
    ) where

import Data.Maybe (listToMaybe)
import qualified Control.Applicative as A ((<|>), many, some)
import Control.Applicative (Alternative (empty))

newtype Parser a = Parser { runParser :: String -> [(String, a)] }

instance Functor Parser where
    fmap :: (a -> b) -> Parser a -> Parser b
    fmap f (Parser p) = Parser $ \inp -> [(rest, f result) | (rest, result) <- p inp]

instance Applicative Parser where
    pure :: a -> Parser a
    pure x = Parser $ \inp -> [(inp, x)]
    (<*>) :: Parser (a -> b) -> Parser a -> Parser b
    Parser pf <*> Parser px = Parser $ \inp -> [ (rest2, result1 result2) | (rest1, result1) <- pf inp
                                                                          , (rest2, result2) <- px rest1 ]

instance Monad Parser where
    (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    (Parser p) >>= f = Parser $ \inp -> [ r | (rest, result) <- p inp
                                            , r <- runParser (f result) rest ]


instance Alternative Parser where
    empty :: Parser a
    empty = Parser $ const []
    (<|>) :: Parser a -> Parser a -> Parser a
    Parser x <|> Parser y = Parser $ \inp ->
        x inp ++ y inp

(<|>) :: Parser a -> Parser a -> Parser a
(<|>) = (A.<|>)

parse :: Parser a -> String -> Maybe a
parse p = fmap snd . listToMaybe . runParser p

result :: a -> Parser a
result = pure

nom :: Parser Char
nom = Parser $ \inp -> case inp of
                           []     -> []
                           (x:xs) -> [(xs, x)]

many :: Parser a -> Parser [a]
many = A.many

some :: Parser a -> Parser [a]
some = A.some

sat :: (Char -> Bool) -> Parser Char
sat p = do
  x <- nom
  if p x then result x else empty

char :: Char -> Parser Char
char c = sat $ \x -> x == c

string :: String -> Parser String
string [] = pure []
string (x:xs) = do
  r <- char x
  rs <- string xs
  result $ r:rs

lowercase :: Parser Char
lowercase = sat $ \c -> 'a' <= c && c <= 'z'

uppercase :: Parser Char
uppercase = sat $ \c -> 'A' <= c && c <= 'Z'

letter :: Parser Char
letter = lowercase <|> uppercase

digit :: Parser Char
digit = sat $ \c -> '0' <= c && c <= '9'

alnum :: Parser Char
alnum = letter <|> digit