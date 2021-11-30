module Day00Spec where
{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec

import Lib

spec :: Spec
spec = do
  describe "Part 1" $ do
    it "can handle given example" $ do
      2 `shouldBe` 3

    it "can solve Part 1" $ do
      pending
      input <- readFile "input"
      pendingWith "need to solve Part 1"
--       doPart1 input `shouldBe` 0
