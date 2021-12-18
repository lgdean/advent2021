module Day18Spec where
{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec

import Day18

spec :: Spec
spec = do
  describe "Part 1" $ do
    it "can handle given example" $ do
      input <- readFile "day18-example-input"
      doPart1 input `shouldBe` 0

    it "can solve Part 1" $ do
      input <- readFile "day18-input"
      doPart1 input `shouldBe` 0
