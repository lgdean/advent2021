module Day19Spec where
{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec

import Data.List (group, sort)

import Day19

spec :: Spec
spec = do
  describe "rotation logic" $ do
    it "creates proper number of distinct rotations" $ do
      length (group $ sort $ rotationsOld (5,1,20)) `shouldBe` 24

  describe "overlap logic" $ do
    it "can handle given 3d example" $ do
      [scanner0, scanner1] <- traverse (\n -> readFile ("day19-example/" ++ show n)) [0,1]
      findOverlapParsing scanner0 scanner1 `shouldMatchList` [(-618,-824,-621),
                                                                       (-537,-823,-458),
                                                                       (-447,-329,318 ),
                                                                       (404,-588,-901 ),
                                                                       (544,-627,-890 ),
                                                                       (528,-643,409  ),
                                                                       (-661,-816,-575),
                                                                       (390,-675,-793 ),
                                                                       (423,-701,434  ),
                                                                       (-345,-311,381 ),
                                                                       (459,-707,401  ),
                                                                       (-485,-357,347 )]

  describe "Part 1" $ do
    it "can handle given example" $ do
      inputs <- traverse (\n -> readFile ("day19-example/" ++ show n)) [0..4]
      doPart1 inputs `shouldBe` -1085 -- TODO this was just to test parsing! remove and use real assertion below
--      doPart1 inputs `shouldBe` 79

    it "can solve Part 1" $ do
      pending
--      doPart1 inputs `shouldBe` 0
