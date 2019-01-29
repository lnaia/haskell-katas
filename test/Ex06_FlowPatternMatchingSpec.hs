module Ex06_FlowPatternMatchingSpec
  ( spec
  ) where

import Test.Hspec

main :: IO ()
main = hspec spec

-- factorial
factorial :: Int -> Int
factorial 0 = 1
factorial 5 = 120
factorial x = x * (factorial (x - 1))

charName :: Char -> [Char]
charName 'a' = "Albert"
charName _ = "What do you mean?"

addVectors :: (Int, Int) -> (Int, Int) -> (Int, Int)
-- addVectors x y = (fst x + fst y, snd x + snd y)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

first :: (Int, Int, Int) -> Int
first (x, _, _) = x
second (_, x, _) = x
third (_, _, x) = x


head' :: [a] -> a
head' (x:_) = x

tell :: [Int] -> [Char]
tell [] = "This list is empty"
tell (x:_) = "This list has one element: 1"

length' :: [Int] -> Int
length' [] = 0
length' (x:xs) =  1 + length' xs

reduce' :: [Int] -> Int
reduce' [] = 0
reduce' (x:xs) =  x + reduce' xs

spec :: Spec
spec = do
  describe "Pattern matching" $ do
    it "can be used in factorial calc" $ do
     factorial 5 `shouldBe` 120
    it "can fail when no default case" $ do
      charName 'a' `shouldBe` "Albert"
      charName '?' `shouldBe` "What do you mean?"
    it "can be used on tuples" $ do
      addVectors (1,2)(3,4) `shouldBe` (4,6)
    it "can be used on triples" $ do
      first (1,2,3) `shouldBe` 1
      second (1,2,3) `shouldBe` 2
      third (1,2,3) `shouldBe` 3
    it "can pattern list comprehensions" $ do
      let xs = [(1,3),(4,3),(2,4),(5,3),(5,6),(3,1)]
      [a+b | (a,b) <- xs] `shouldBe` [4,7,6,8,11,4]
    it "can be used for the head function" $ do
      head' [2,3,4] `shouldBe` 2
      head' "Hello" `shouldBe` 'H'
    it "can safely process a list" $ do
      tell [] `shouldBe` "This list is empty"
      tell [1] `shouldBe` "This list has one element: 1"
      -- tell [1,2] `shouldBe` "This list has two elements: 1 and 2"
      -- tell [1,2,3] `shouldBe` "This list is too long"
    it "can count elements in list with recursion" $ do
      length' [] `shouldBe` 0
      length' [1,2,3] `shouldBe` 3
    it "can reduce add a list" $ do
      reduce' [] `shouldBe` 0
      reduce' [1,2,3] `shouldBe` 6
    it "can hold the original item with pattern" $ do
      firstLetter "" `shouldBe` "Empty string, whoops!"
      firstLetter "Dracula" `shouldBe` "The first letter of Dracula is D"


      firstLetter :: [Char] -> [Char]
      firstLetter [] = "Empty string, whoops!"
