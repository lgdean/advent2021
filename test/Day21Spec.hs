module Day21Spec where
{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec

import Day21

spec :: Spec
spec = do
  describe "Part 1" $ do
    it "can handle given example" $ do
      doPart1 4 8 `shouldBe` 739785

    it "can solve Part 1" $ do
      doPart1 8 2 `shouldBe` 513936
