module SchemeGrammar
   ( Expression(..)
   )

where

data Expression =
                  NullLiteral
                | BoolLiteral Bool
                | IntLiteral Integer
                | StringLiteral String
                | Identifier String
                | ArithmeticOp Char
                | CompOp Char
                | IfStatement Expression Expression Expression
                | DefineStatement String [String] Expression
                | FunctionCall Expression [Expression]

  deriving (Eq, Show)
