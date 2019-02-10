import Test.Hspec
import Control.Exception (evaluate)
import Parser (parse)
import SchemeParser
import SchemeGrammar

main :: IO ()
main = hspec $ do
  describe "nullLiteral" $ do
    it "parses \"null\"" $ do
      parse nullLiteral "asdf" `shouldBe` Nothing
      parse nullLiteral "null" `shouldBe` Just NullLiteral

  describe "boolLiteral" $ do
    it "parses #t" $ do
      parse boolLiteral "#t" `shouldBe` Just (BoolLiteral True)
    it "parses #f" $ do
      parse boolLiteral "#f" `shouldBe` Just (BoolLiteral False)
    it "does not match anything else" $ do
      parse nullLiteral "asdf" `shouldBe` Nothing

  describe "intLiteral" $ do
    it "parses 1 digit" $ do
      parse intLiteral "1" `shouldBe` Just (IntLiteral 1)
    it "parses many digits" $ do
      parse intLiteral "123" `shouldBe` Just (IntLiteral 123)

  describe "stringLiteral" $ do
    it "parses strings" $ do
      parse stringLiteral "\"\""       `shouldBe` Just (StringLiteral "")
      parse stringLiteral "\"asdf\""   `shouldBe` Just (StringLiteral "asdf")
      parse stringLiteral "\"asdf\"\"" `shouldBe` Just (StringLiteral "asdf")

  describe "identifier" $ do
    it "does not match an empty string" $ do
      parse identifier "" `shouldBe` Nothing
    it "matches a 1 letter identifier" $ do
      parse identifier "a" `shouldBe` Just (Identifier "a")
    it "matches a multiple letter identifiers" $ do
      parse identifier "asdf123 def123" `shouldBe` Just (Identifier "asdf123")
    it "does not match a string starting with a number" $ do
      parse identifier "1adsf" `shouldBe` Nothing

  describe "expression" $ do
    it "does not match an empty string" $ do
      parse expression "" `shouldBe` Nothing
    it "matches a single term" $ do
      parse expression "asd" `shouldBe` Just (Identifier "asd")
    it "matches a non-nested expression" $ do
      parse expression "(a b c)" `shouldBe` Just (FunctionCall (Identifier "a")
                                                               [ Identifier "b"
                                                               , Identifier "c"])

  describe "functionCall" $ do
    it "does match a function call" $ do
      parse functionCall "(a b c)" `shouldBe` Just (FunctionCall (Identifier "a")
                                                                 [ Identifier "b"
                                                                 , Identifier "c"])