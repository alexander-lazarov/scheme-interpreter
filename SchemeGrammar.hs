module SchemeGrammar
   ( Expression(..)
   )

where

data Expression =
                  NullLiteral
                | BooleanLiteral Bool
                | IntegerLiteral Integer
                | StringLiteral String
                | Identifier String
                | ArithmeticOperator Char
                | IfStatement Expression Expression Expression
                | Header [Expression]
                | DefineStatement Expression Expression
                | FunctionCall [Expression]

  deriving (Eq, Show)
