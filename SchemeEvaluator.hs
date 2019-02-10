module SchemeEvaluator
  ( eval
  , dispatch
  )

where

import SchemeGrammar

eval :: Expression -> Expression
eval NullLiteral                      = NullLiteral
eval (BoolLiteral b)                  = BoolLiteral b
eval (IntLiteral i)                   = IntLiteral i
eval (StringLiteral s)                = StringLiteral s
eval (ArithmeticOp o)                 = ArithmeticOp o
eval (FunctionCall function operands) =
  dispatch functionEvaluated operandsEvaluated
  where
    functionEvaluated = eval function
    operandsEvaluated = fmap eval operands

toInt :: Expression -> Integer
toInt (IntLiteral i) = i
toInt _              = error "Cannot get the integer value of a non-integer"

arithmeticOpDispatch :: Char -> (Integer -> Integer -> Integer)
arithmeticOpDispatch '+' = (+)
arithmeticOpDispatch '-' = (-)
arithmeticOpDispatch '*' = (*)
arithmeticOpDispatch '/' = quot

dispatch :: Expression -> [Expression] -> Expression
dispatch (ArithmeticOp operator) operands =
  IntLiteral $ foldl1 operatorFunction operandValues
  where
    operatorFunction = arithmeticOpDispatch operator
    operandValues = fmap (toInt . eval) operands
