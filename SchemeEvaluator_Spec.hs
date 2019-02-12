import Test.Hspec
import Control.Exception (evaluate)
import SchemeGrammar
import SchemeEvaluator

main :: IO ()
main = hspec $ do
  describe "eval" $ do
    it "evalulates null" $ do
      eval [] NullLiteral `shouldBe` NullLiteral
    it "evaluates arithmetic expression calls" $ do
      eval [] (FunctionCall (ArithmeticOp '+') [IntLiteral 2, IntLiteral 3])
         `shouldBe` IntLiteral 5
    it "evaluates ifs" $ do
      eval [] (IfStatement (BoolLiteral True) (IntLiteral 1) (IntLiteral 2))
         `shouldBe` IntLiteral 1
      eval [] (IfStatement (BoolLiteral False) (IntLiteral 1) (IntLiteral 2))
         `shouldBe` IntLiteral 2
    it "evaluates identifiers" $ do
      eval [("foo", NullLiteral)] (Identifier "foo") `shouldBe` NullLiteral

  describe "dispatch" $ do
    it "evalulates arithmetic expressions" $ do
      dispatch [] (ArithmeticOp '+') [IntLiteral 2, IntLiteral 3]
         `shouldBe` IntLiteral 5
    it "evalulates comparsion expressions" $ do
      dispatch [] (CompOp '=') [IntLiteral 2, IntLiteral 3]
         `shouldBe` BoolLiteral False

