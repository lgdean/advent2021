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
