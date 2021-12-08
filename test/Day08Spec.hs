module Day08Spec where
{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec

import Day08

spec :: Spec
spec = do
  describe "Part 1" $ do
    it "can handle given example" $ do
      input <- readFile "day08-example-input"
      doPart1 input `shouldBe` 26

    it "can solve Part 1" $ do
      input <- readFile "day08-input"
      doPart1 input `shouldBe` 514
