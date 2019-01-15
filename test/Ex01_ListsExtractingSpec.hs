module Ex01_ListsExtractingSpec
  ( spec
  ) where

import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Extracting Portion of List" $ do
    it "finds the first element in a list" $ do
      head [1,2,3,4,5] `shouldBe` 1
    it "finds the tail part of a list" $ do
      tail [1, 2, 3, 4, 5] `shouldBe` [2, 3, 4, 5]
    it "finds the last element in a list" $ do
      head(reverse([1,2,3,4,5])) `shouldBe` 5
    it "extracts the elements except the last one from a list" $ do
      reverse(tail(reverse([1, 2, 3, 4, 5]))) `shouldBe` [1, 2, 3, 4]
    it "takes elements from a list" $ do
      take 7 ['a'..'z'] `shouldBe` "abcdefg"
      take 3 [1..5] `shouldBe` [1, 2, 3]
      take 5 (enumFromTo 10 100) `shouldBe` [10, 11, 12, 13, 14]
      take 5 (enumFrom 10) `shouldBe` [10, 11, 12, 13, 14]
    it "can drop elements from a list" $ do
      reverse(take 5 (reverse([1 .. 10]))) `shouldBe` [6, 7, 8, 9, 10]
      reverse(take 4 (reverse(['a' .. 'g'])))  `shouldBe` "defg"
      reverse(take 5 (reverse((enumFromTo 10 20)))) `shouldBe` [16, 17, 18, 19, 20]
    it "can split a collection" $ do
      splitAt 5 [1 .. 10] `shouldBe` ([1, 2, 3, 4, 5], [6, 7, 8, 9, 10])
    it "can take with a while" $ do
      take 2 [1 .. 10] `shouldBe` [1, 2]
      take 3 (enumFromTo 5 15) `shouldBe` [5, 6, 7]
      take 1 "abracadabra" `shouldBe` "a"
    it "can drop while" $ do
      snd(splitAt 4 ([1 .. 10])) `shouldBe` [5, 6, 7, 8, 9, 10]
      snd(splitAt 1 "abracadabra") `shouldBe` "bracadabra"
