import Parser
import SchemeGrammar
import SchemeParser
import SchemeEvaluator

evalStr :: String -> Maybe Expression
evalStr inp = do
  parsed <- parse program inp
  evaluated <- Just $ evalProgram [] parsed
  return $ evaluated

getLines :: IO [String]
getLines = lines <$> getContents

main :: IO ()
main = do
    lines <- getLines

    let nonEmptyLines = takeWhile (not . null) lines
    let inp = foldl1 (++) $ map (++ "\n") nonEmptyLines :: String

    putStrLn $ show $ evalStr inp
