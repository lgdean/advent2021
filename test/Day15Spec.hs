module Day15Spec where
{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec

import Day15

spec :: Spec
spec = do
  describe "Part 1" $ do
    it "can handle given example" $ do
      input <- readFile "day15-example-input"
      doPart1 input `shouldBe` 40

    it "can solve Part 1" $ do
      input <- readFile "day15-input"
      doPart1 input `shouldBe` 621

  describe "Part 2" $ do
    it "can handle given example" $ do
      input <- readFile "day15-example-input"
      doPart2 input `shouldBe` 315

    it "can solve Part 2" $ do
      input <- readFile "day15-input"
      doPart2 input `shouldBe` 2904
