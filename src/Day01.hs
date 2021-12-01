module Day01
    ( countIncreases,
      doPart1
    ) where

import Debug.Trace (trace)

countIncreases :: [Int] -> Int
countIncreases input =
  length $ filter id $ zipWith (>) (tail input) input

doPart1 :: [Char] -> Int
doPart1 input =
  let inputInts = map read $ lines input
  in countIncreases inputInts