module Day04Spec where
{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec

import Day04

spec :: Spec
spec = do
  describe "Part 1" $ do
    it "knows a winning row when it sees one" $ do
      winning [[(True, 1), (True, 2), (True, 3), (True, 4), (True, 5)],
               [(True, 2), (False, 2), (True, 3), (True, 4), (False, 5)]] `shouldBe` True

    it "knows a winning column when it sees one" $ do
      winning [[(False, 1), (True, 2), (True, 3), (True, 4), (True, 5)],
               [(True, 1), (False, 2), (True, 3), (True, 4), (True, 5)],
               [(True, 1), (True, 2), (True, 3), (False, 4), (True, 5)],
               [(True, 1), (True, 2), (True, 3), (True, 4), (False, 5)],
               [(True, 2), (False, 2), (True, 3), (True, 4), (False, 5)]] `shouldBe` True

    it "can handle given example" $ do
      input <- readFile "day04-example-input"
      doPart1 [7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1] input `shouldBe` 4512

    it "can solve Part 1" $ do
      input <- readFile "day04-input"
      doPart1
       [15,61,32,33,87,17,56,73,27,83,0,18,43,8,86,37,40,6,93,25,14,68,64,57,39,46,55,13,21,72,51,81,78,79,52,65,36,92,97,28,9,24,22,69,70,42,3,4,63,50,91,16,41,94,77,85,49,12,76,67,11,62,99,54,95,1,74,34,88,89,82,48,84,98,58,44,5,53,7,19,29,30,35,26,31,10,60,59,80,71,45,38,20,66,47,2,23,96,90,75]
       input `shouldBe` 58412

  describe "Part 2" $ do
    it "can handle given example" $ do
      input <- readFile "day04-example-input"
      doPart2
       [7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1]
       input `shouldBe` 1924

    it "can solve Part 2" $ do
      input <- readFile "day04-input"
      doPart2
       [15,61,32,33,87,17,56,73,27,83,0,18,43,8,86,37,40,6,93,25,14,68,64,57,39,46,55,13,21,72,51,81,78,79,52,65,36,92,97,28,9,24,22,69,70,42,3,4,63,50,91,16,41,94,77,85,49,12,76,67,11,62,99,54,95,1,74,34,88,89,82,48,84,98,58,44,5,53,7,19,29,30,35,26,31,10,60,59,80,71,45,38,20,66,47,2,23,96,90,75]
       input `shouldBe` 10030
