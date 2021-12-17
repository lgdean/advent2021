module Day17Spec where
{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec

import Data.Range

import Day17

spec :: Spec
spec = do
  describe "Part 1" $ do
    it "can handle given example" $ do
      doPart1 (20 +=+ 30) ((-10) +=+ (-5)) `shouldBe` 45

    it "can solve Part 1" $ do
      doPart1 (155 +=+ 182) ((-117) +=+ (-67)) `shouldBe` 6786

  describe "Part 2" $ do
    it "can handle given example" $ do
      doPart2 (20 +=+ 30) ((-10) +=+ (-5)) `shouldBe` 112

    it "can solve Part 1" $ do
      doPart2 (155 +=+ 182) ((-117) +=+ (-67)) `shouldBe` 2313
