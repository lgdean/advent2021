module Day09Spec where
{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec

import Day09

spec :: Spec
spec = do
  describe "Part 1" $ do
    it "can handle given example" $ do
      input <- readFile "day09-example-input"
      doPart1 input `shouldBe` 15

    it "can solve Part 1" $ do
      input <- readFile "day09-input"
      doPart1 input `shouldBe` 468

--  describe "Part 2" $ do
--    it "can handle given example" $ do
--      input <- readFile "day09-example-input"
--      doPart2 input `shouldBe` 0
--
--    it "can solve Part 2" $ do
--      input <- readFile "day09-input"
--      doPart2 input `shouldBe` 0
