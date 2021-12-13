module Day13Spec where
{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec

import Day13

spec :: Spec
spec = do
  describe "Part 1" $ do
    it "can handle given example" $ do
      input <- readFile "day13-example-input"
      doPart1 7 input `shouldBe` 17

    it "can solve Part 1" $ do
      input <- readFile "day13-input"
      doPart1 655 input `shouldBe` 759
