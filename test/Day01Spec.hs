module Day01Spec where
{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec

import Day01

spec :: Spec
spec = do
  describe "Part 1" $ do
    it "can handle given example" $ do
      countIncreases [199,
                      200,
                      208,
                      210,
                      200,
                      207,
                      240,
                      269,
                      260,
                      263] `shouldBe` 7

    it "can solve Part 1" $ do
      input <- readFile "input"
      doPart1 input `shouldBe` 1184

  describe "Part 2" $ do
    it "can handle given example" $ do
      countTripleIncreases [199,
                      200,
                      208,
                      210,
                      200,
                      207,
                      240,
                      269,
                      260,
                      263] `shouldBe` 5

    it "can solve Part 2" $ do
      input <- readFile "input"
      doPart2 input `shouldBe` 1158
