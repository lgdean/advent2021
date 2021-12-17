module Day17Spec where
{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec

import Day17

spec :: Spec
spec = do
  describe "Part 1" $ do
    it "can handle given example" $ do
      input <- readFile "day17-example-input"
      pending
      doPart1 input `shouldBe` 0

    it "can solve Part 1" $ do
      input <- readFile "day17-input"
      pending
      doPart1 input `shouldBe` 0
