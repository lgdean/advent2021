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

  describe "Part 2" $ do
    it "can decode one example number" $ do
      decodeEntry (words "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab") "cdfeb" `shouldBe` 5

    it "can handle given example" $ do
      input <- readFile "day08-example-input"
      doPart2 input `shouldBe` 61229

    it "can solve Part 2" $ do
      input <- readFile "day08-input"
      doPart2 input `shouldBe` 1012272
