module Day14Spec where
{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec

import Day14

spec :: Spec
spec = do
  describe "Part 1" $ do
    it "can handle given example" $ do
      input <- readFile "day14-example-input"
      doPart1 "NNCB" input `shouldBe` 1588

    it "can solve Part 1" $ do
      input <- readFile "day14-input"
      doPart1 "SVCHKVFKCSHVFNBKKPOC" input `shouldBe` 3058

  describe "Part 2" $ do
    it "can handle given example" $ do
      input <- readFile "day14-example-input"
      doPart2 "NNCB" input `shouldBe` 2188189693529

    it "can solve Part 1" $ do
      input <- readFile "day14-input"
      doPart2 "SVCHKVFKCSHVFNBKKPOC" input `shouldBe` 3447389044530
