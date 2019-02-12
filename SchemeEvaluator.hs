module SchemeEvaluator
  ( eval
  , dispatch
  )

where

import SchemeGrammar

type Binding = (String, Expression)

toInt :: Expression -> Integer
toInt (IntLiteral i) = i
toInt _              = error "Cannot get the integer value of a non-integer"

arithmeticOpDispatch :: Char -> (Integer -> Integer -> Integer)
arithmeticOpDispatch '+' = (+)
arithmeticOpDispatch '-' = (-)
arithmeticOpDispatch '*' = (*)
arithmeticOpDispatch '/' = quot

eval :: [Binding] -> Expression -> Expression
eval _        NullLiteral                      = NullLiteral
eval _        (BoolLiteral b)                  = BoolLiteral b
eval _        (IntLiteral i)                   = IntLiteral i
eval _        (StringLiteral s)                = StringLiteral s
eval _        (ArithmeticOp o)                 = ArithmeticOp o
eval bindings (FunctionCall function operands) =
  dispatch bindings functionEvaluated operandsEvaluated
  where
    eval'             = eval bindings
    functionEvaluated = eval' function
    operandsEvaluated = fmap eval' operands
eval bindings (Identifier i)                  =
  case binding of Nothing -> error $ "Unknown binding " ++ i
                  Just e  -> e
  where
    binding = lookup i bindings

dispatch :: [Binding] -> Expression -> [Expression] -> Expression
dispatch bindings (ArithmeticOp operator) operands =
  IntLiteral $ foldl1 operatorFunction operandValues
  where
    eval'            = eval bindings
    operatorFunction = arithmeticOpDispatch operator
    operandValues    = fmap (toInt . eval') operands
