module Day05Spec where
{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec

import Day05

spec :: Spec
spec = do
  describe "Part 2" $ do
    it "can handle given example" $ do
      input <- readFile "day05-example-input"
      doPart1 input `shouldBe` 12

    it "can solve Part 2" $ do
      input <- readFile "day05-input"
      doPart1 input `shouldBe` 19851
