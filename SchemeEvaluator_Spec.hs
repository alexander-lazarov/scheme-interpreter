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
  describe "dispatch" $ do
    it "evalulates arithmetic expressions" $ do
      dispatch [] (ArithmeticOp '+') [IntLiteral 2, IntLiteral 3]
         `shouldBe` IntLiteral 5
