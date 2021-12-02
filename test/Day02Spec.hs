module Day02Spec where
{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec

import Day02

spec :: Spec
spec = do
  describe "Part 1" $ do
    it "can handle given example" $ do
      positionProduct ["forward 5",
                       "down 5",
                       "forward 8",
                       "up 3",
                       "down 8",
                       "forward 2"] `shouldBe` 150

    it "can solve Part 1" $ do
      input <- readFile "day02-input"
      doPart1 input `shouldBe` 2120749

--  describe "Part 2" $ do
--    it "can handle given example" $ do
--      pending
--
--    it "can solve Part 2" $ do
--      input <- readFile "day02-input"
--      doPart2 input `shouldBe` 0
