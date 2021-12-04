module Day03Spec where
{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec

import Day03

spec :: Spec
spec = do
  describe "Part 1" $ do
    it "can calculate gamma rate for given example" $ do
      gammaRate   ["00100",
                   "11110",
                   "10110",
                   "10111",
                   "10101",
                   "01111",
                   "00111",
                   "11100",
                   "10000",
                   "11001",
                   "00010",
                   "01010"] `shouldBe` 22

    it "can handle given example" $ do
      part1answer ["00100",
                   "11110",
                   "10110",
                   "10111",
                   "10101",
                   "01111",
                   "00111",
                   "11100",
                   "10000",
                   "11001",
                   "00010",
                   "01010"] `shouldBe` 198

    it "can solve Part 1" $ do
      input <- readFile "day03-input"
      doPart1 input `shouldBe` 3901196

  describe "Part 2" $ do
    it "can calculate oxygen generator rating for given example" $ do
      oxygenGeneratorRating ["00100",
                   "11110",
                   "10110",
                   "10111",
                   "10101",
                   "01111",
                   "00111",
                   "11100",
                   "10000",
                   "11001",
                   "00010",
                   "01010"] `shouldBe` "10111" -- 23

    it "can handle given example" $ do
      part2answer ["00100",
                   "11110",
                   "10110",
                   "10111",
                   "10101",
                   "01111",
                   "00111",
                   "11100",
                   "10000",
                   "11001",
                   "00010",
                   "01010"] `shouldBe` 230

    it "can solve Part 2" $ do
      input <- readFile "day03-input"
      doPart2 input `shouldBe` 4412188
