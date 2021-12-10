module Day10Spec where
{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec

import Day10

spec :: Spec
spec = do
  describe "Part 1" $ do
    it "can handle given example" $ do
      input <- readFile "day10-example-input"
      doPart1 input `shouldBe` 26397

    it "can solve Part 1" $ do
      input <- readFile "day10-input"
      doPart1 input `shouldBe` 319329

  describe "Part 2" $ do
    it "can handle given example" $ do
      input <- readFile "day10-example-input"
      doPart2 input `shouldBe` 288957

    it "can solve Part 2" $ do
      input <- readFile "day10-input"
      doPart2 input `shouldBe` 3515583998
