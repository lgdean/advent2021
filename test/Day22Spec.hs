module Day22Spec where
{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec

import Day22

spec :: Spec
spec = do
  describe "Part 1" $ do
    it "can handle very simple given example" $ do
      input <- readFile "day22-simple-example-input"
      doPart1 input `shouldBe` 39

    it "can handle given example" $ do
      input <- readFile "day22-example-input"
      doPart1 input `shouldBe` 590784

    it "can solve Part 1" $ do
      input <- readFile "day22-input"
      doPart1 input `shouldBe` 570915
