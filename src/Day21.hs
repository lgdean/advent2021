module Day21
    (
--      doPart2,
      doPart1
    ) where

import Data.List.Split (chunksOf)

import Debug.Trace (trace)


doPart1 :: Int -> Int -> Int
doPart1 p1 p2 =
  let result = playGame (0, p1) (0, p2) (map sum $ chunksOf 3 dieRolls)
      nTurns = length result - 2
      nDieRolls = 3 * nTurns
  in trace (show nDieRolls) $ nDieRolls * fst (result !! nTurns)

playGame :: (Int, Int) -> (Int, Int) -> [Int] -> [(Int, Int)]
playGame (s,p) x dice
  | s >= 1000 = [(s,p)]
  | otherwise = (s,p) : playGame x (advance (s,p) (head dice)) (tail dice)

advance :: (Int, Int) -> Int -> (Int, Int)
advance (score, position) roll =
  let newPos = ((position + roll - 1) `mod` 10) + 1
  in (score + newPos, newPos)

dieRolls :: [Int]
dieRolls = cycle [1..100]
