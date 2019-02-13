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

toBool :: Expression -> Bool
toBool (BoolLiteral b) = b
toBool _               = error "Cannot cast to bool"

arithmeticOpDispatch :: Char -> (Integer -> Integer -> Integer)
arithmeticOpDispatch '+' = (+)
arithmeticOpDispatch '-' = (-)
arithmeticOpDispatch '*' = (*)
arithmeticOpDispatch '/' = quot

compOpDispatch :: Char -> (Integer -> Integer -> Bool)
compOpDispatch '=' = (==)
compOpDispatch '<' = (<)
compOpDispatch '>' = (>)

eval :: [Binding] -> Expression -> Expression
eval _        NullLiteral                      = NullLiteral
eval _        (BoolLiteral b)                  = BoolLiteral b
eval _        (IntLiteral i)                   = IntLiteral i
eval _        (StringLiteral s)                = StringLiteral s
eval _        (ArithmeticOp o)                 = ArithmeticOp o
eval _        (CompOp o)                       = CompOp o
eval _        (DefineStatement n b p)          = DefineStatement n b p
eval bindings (IfStatement p t f)              =
  if toBool pval then tval else fval
  where
    eval'     = eval bindings
    pval      = eval' p
    tval      = eval' t
    fval      = eval' f
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

dispatch bindings (CompOp op) (x:y:[]) =
  BoolLiteral $ op' xv yv
  where
    eval' = eval bindings
    op'   = compOpDispatch op
    xv    = toInt x
    yv    = toInt y
dispatch bindings (CompOp operator) _  = error "Arity mismatch"
dispatch bindings (DefineStatement _ defArgs body) callArgs =
  if length defArgs /= length callArgs then
    error "Arity mismatch"
  else
    eval' body
  where
    boundParams = zip defArgs callArgs
    newBindings = boundParams ++ bindings
    eval' = eval newBindings

dispatch _ _ _                        = error "Dispatch error"
