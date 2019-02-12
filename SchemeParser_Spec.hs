import Test.Hspec
import Control.Exception (evaluate)
import Parser (parse)
import SchemeGrammar
import SchemeParser

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
    it "parses many digits" $ do
      parse intLiteral "-123" `shouldBe` Just (IntLiteral $ -123)

  describe "stringLiteral" $ do
    it "parses strings" $ do
      parse stringLiteral "\"\""       `shouldBe` Just (StringLiteral "")
      parse stringLiteral "\"asdf\""   `shouldBe` Just (StringLiteral "asdf")
      parse stringLiteral "\"asdf\"\"" `shouldBe` Just (StringLiteral "asdf")

  describe "arithmeticOp" $ do
    it "parses arithmetic ops" $ do
      parse arithmeticOp "+" `shouldBe` Just (ArithmeticOp '+')
      parse arithmeticOp "-" `shouldBe` Just (ArithmeticOp '-')

  describe "compOp" $ do
    it "parses comparsion ops" $ do
      parse compOp "=" `shouldBe` Just (CompOp '=')

  describe "identifier" $ do
    it "does not match an empty string" $ do
      parse identifier "" `shouldBe` Nothing
    it "matches a 1 letter identifier" $ do
      parse identifier "a" `shouldBe` Just (Identifier "a")
    it "matches a multiple letter identifiers" $ do
      parse identifier "asdf123 def123" `shouldBe` Just (Identifier "asdf123")
    it "does not match a string starting with a number" $ do
      parse identifier "1adsf" `shouldBe` Nothing

  describe "ifStatement" $ do
    it "matches if statments" $ do
      parse ifStatement "(if c 1 2)" `shouldBe`
          Just (IfStatement (Identifier "c") (IntLiteral 1) (IntLiteral 2))

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

    it "ignores whitespace" $ do
      parse functionCall "  (a b\r\n   c  )\n" `shouldBe` Just (FunctionCall (Identifier "a")
                                                                           [ Identifier "b"
                                                                           , Identifier "c"])
