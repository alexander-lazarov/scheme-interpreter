import Test.Hspec
import Control.Exception (evaluate)
import Parser

main :: IO ()
main = hspec $ do
  describe "result" $ do
    it "returns a parser that always returns the value passed" $ do
      parse parserA ""    `shouldBe` Just 'a'
      parse parserA "ads" `shouldBe` Just 'a'

  describe "empty" $ do
    it "is a parser that always returns nothing" $ do
      (parse empty ""    :: Maybe Char) `shouldBe` Nothing
      (parse empty "asd" :: Maybe Char) `shouldBe` Nothing

  describe "nom" $ do
    it "consumes a single char" $ do
      parse nom ""    `shouldBe` Nothing
      parse nom "asd" `shouldBe` Just 'a'
      parse nom "dsa" `shouldBe` Just 'd'

  describe "many" $ do
    it "matches 0" $ do
      parse (many nom) ""            `shouldBe` Just ""
      parse (many $ char 'a') "sdf"  `shouldBe` Just ""
    it "matches 1 or more" $ do
      parse (many $ char 'a') "aadf" `shouldBe` Just "aa"
      parse (many nom) "asdf"        `shouldBe` Just "asdf"

  describe "many1" $ do
    it "does not match 0" $ do
      parse (some nom) ""            `shouldBe` Nothing
      parse (some $ char 'a') "sdf"  `shouldBe` Nothing
    it "matches 1 or more" $ do
      parse (some nom) "asdf"        `shouldBe` Just "asdf"
      parse (some $ char 'a') "aasd" `shouldBe` Just "aa"

  describe "sat" $ do
    it "returns a single char if pred is true" $ do
      parse (sat (const True)) ""    `shouldBe` Nothing
      parse (sat (const True)) "asd" `shouldBe` Just 'a'
      parse (sat (const True)) "dsa" `shouldBe` Just 'd'
    it "returns nothing if pred is not true" $ do
      parse (sat (const False)) ""    `shouldBe` Nothing
      parse (sat (const False)) "asd" `shouldBe` Nothing
      parse (sat (const False)) "dsa" `shouldBe` Nothing

  describe "char" $ do
    it "matches a char" $ do
      parse (char 'a') ""    `shouldBe` Nothing
      parse (char 'a') "asd" `shouldBe` Just 'a'
      parse (char 'a') "dsa" `shouldBe` Nothing

  describe "string" $ do
    it "matches the beginning of the string only" $ do
      parse (string "") ""         `shouldBe` Just ""
      parse (string "asd") "asdad" `shouldBe` Just "asd"
      parse (string "ddd") "asdad" `shouldBe` Nothing
      parse (string "sd")  "asd"   `shouldBe` Nothing

  describe "lowercase" $ do
    it "matches lowercase letters" $ do
      parse lowercase "a" `shouldBe` Just 'a'
      parse lowercase "k" `shouldBe` Just 'k'
    it "does not match upper case letters" $ do
      parse lowercase ""  `shouldBe` Nothing
      parse lowercase "A" `shouldBe` Nothing
      parse lowercase "5" `shouldBe` Nothing
      parse lowercase " " `shouldBe` Nothing

  describe "uppercase" $ do
    it "matches uppercase letters" $ do
      parse uppercase "A" `shouldBe` Just 'A'
      parse uppercase "K" `shouldBe` Just 'K'
    it "does not match upper case letters" $ do
      parse uppercase ""  `shouldBe` Nothing
      parse uppercase "a" `shouldBe` Nothing
      parse uppercase "5" `shouldBe` Nothing
      parse uppercase " " `shouldBe` Nothing

  describe "letter" $ do
    it "matches letters" $ do
      parse letter "a" `shouldBe` Just 'a'
      parse letter "A" `shouldBe` Just 'A'
      parse letter "k" `shouldBe` Just 'k'
      parse letter "K" `shouldBe` Just 'K'
    it "does not match upper case letters" $ do
      parse letter ""  `shouldBe` Nothing
      parse letter "5" `shouldBe` Nothing
      parse letter " " `shouldBe` Nothing

  where
    parserA = result 'a'
