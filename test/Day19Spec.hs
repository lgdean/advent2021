module Day19Spec where
{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec

import Day19

spec :: Spec
spec = do
  describe "Part 1" $ do
    it "can handle given example" $ do
      inputs <- traverse (\n -> readFile ("day19-example/" ++ show n)) [0..4]
      doPart1 inputs `shouldBe` -1085 -- TODO this was just to test parsing! remove and use real assertion below
--      doPart1 inputs `shouldBe` 79

    it "can solve Part 1" $ do
      pending
--      doPart1 inputs `shouldBe` 0
