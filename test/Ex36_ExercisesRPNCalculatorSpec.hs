module Ex36_ExercisesRPNCalculatorSpec
  ( spec
  ) where

import Test.Hspec

main :: IO ()
main = hspec spec

-- foldingFunction :: (Num a, Read a) => [a] -> String -> [a]
-- solveRPN :: (Num a, Read a) => String -> a

spec :: Spec
spec =
  describe "reverse polish notation" $ do
    it "uses a foldingFunction" $ do
      pending
      -- foldl foldingFunction [] ["1","2","+"] `shouldBe` [3]
      -- foldl foldingFunction [] ["2","3","*"] `shouldBe` [6]
    it "calculates simple addition" $ do
      pending
      -- solveRPN "1 2 +" `shouldBe` 3
    it "calculates simple multiplication" $ do
      pending
      -- solveRPN "2 3 *" `shouldBe` 6
    it "calculates more complex expressions" $ do
      pending
      -- solveRPN "10 4 3 + 2 * -" `shouldBe` (-4)
