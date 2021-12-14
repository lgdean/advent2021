module Day13Spec where
{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec

import Day13

spec :: Spec
spec = do
  describe "Part 1" $ do
    it "can handle given example" $ do
      input <- readFile "day13-example-input"
      doPart1Up 7 input `shouldBe` 17

    it "can solve Part 1" $ do
      input <- readFile "day13-input"
      doPart1Left 655 input `shouldBe` 759

  describe "Part 2" $ do
    it "can handle given example" $ do
      input <- readFile "day13-example-input"
      doPart2 [foldUp 7, foldLeft 5] input `shouldBe` 16

    it "can solve Part 1" $ do
      input <- readFile "day13-input"
      doPart2 [foldLeft 655,
               foldUp 447,
               foldLeft 327,
               foldUp 223,
               foldLeft 163,
               foldUp 111,
               foldLeft 81,
               foldUp 55,
               foldLeft 40,
               foldUp 27,
               foldUp 13,
               foldUp 6]
                input `shouldBe` 102
