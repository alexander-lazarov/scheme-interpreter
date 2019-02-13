import Parser
import SchemeGrammar
import SchemeParser
import SchemeEvaluator

evalStr :: String -> Maybe Expression
evalStr inp = do
  parsed <- parse expression inp
  Just $ eval [] parsed

main :: IO ()
main = do
    line <- getLine
    putStrLn $ show $ evalStr line
