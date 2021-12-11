module Day11Spec where
{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec

import Day11

spec :: Spec
spec = do
  describe "Part 1" $ do
    it "can handle given example" $ do
      input <- readFile "day11-example-input"
      doPart1 input `shouldBe` 1656

    it "can solve Part 1" $ do
      input <- readFile "day11-input"
      doPart1 input `shouldBe` 1679

  describe "Part 2" $ do
    it "can handle given example" $ do
      input <- readFile "day11-example-input"
      doPart2 input `shouldBe` 195

    it "can solve Part 2" $ do
      input <- readFile "day11-input"
      doPart2 input `shouldBe` 519
