module Day18Spec where
{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec

import Day18

spec :: Spec
spec = do
  describe "magnitude and parsing" $ do
    it "can handle simple example" $ do
      magnitude (parseNum "[9,1]") `shouldBe` 29
    it "can handle first recursive example" $ do
      magnitude (parseNum "[[9,1],[1,9]]") `shouldBe` 129
    it "can handle a complext example" $ do
      magnitude (parseNum "[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]") `shouldBe` 3488

  describe "Part 1" $ do
    it "can handle given example" $ do
      input <- readFile "day18-example-input"
      pending
      doPart1 input `shouldBe` 4140

    it "can solve Part 1" $ do
      input <- readFile "day18-input"
      pending
      doPart1 input `shouldBe` 0
