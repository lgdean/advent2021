module Day01
    ( countIncreases,
      countTripleIncreases,
      doPart1,
      doPart2
    ) where

import Debug.Trace (trace)

countIncreases :: [Int] -> Int
countIncreases input =
  length $ filter id $ zipWith (>) (tail input) input

doPart1 :: [Char] -> Int
doPart1 input =
  let inputInts = map read $ lines input
  in countIncreases inputInts

countTripleIncreases :: [Int] -> Int
countTripleIncreases input =
  let triples = zip3 input (tail input) (tail $ tail input)
      sums = zipWith (+) input (tail input)
      actualSums = zipWith (+) sums (tail $ tail input)
  in countIncreases actualSums

doPart2 :: [Char] -> Int
doPart2 input =
  let inputInts = map read $ lines input
  in countTripleIncreases inputInts
