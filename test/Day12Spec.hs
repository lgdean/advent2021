module Day12Spec where
{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec

import Day12

spec :: Spec
spec = do
  describe "Part 1" $ do
    it "can handle given example" $ do
      input <- readFile "day12-example-input"
      doPart1 input `shouldBe` 10

    it "can solve Part 1" $ do
      input <- readFile "day12-input"
      doPart1 input `shouldBe` 3563
