module Day01
    ( countIncreases,
      countTripleIncreases,
      doPart1,
      doPart2
    ) where

import Debug.Trace (trace)

import Lib

countIncreases :: [Int] -> Int
countIncreases input =
  length $ filter id $ zipWith (>) (tail input) input

doPart1 :: [Char] -> Int
doPart1 input =
  let inputInts = readIntLines input
  in countIncreases inputInts

countTripleIncreases :: [Int] -> Int
countTripleIncreases input =
  length $ filter id $ zipWith (>) (drop 3 input) input

doPart2 :: [Char] -> Int
doPart2 input =
  let inputInts = readIntLines input
  in countTripleIncreases inputInts
