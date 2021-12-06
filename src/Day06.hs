module Day06
    (
--      doPart2,
      howManyAfter,
      doPart1
    ) where

import Data.List (group, sort)
import Data.List.Split (splitOn)

import Debug.Trace (trace)


howManyAfter :: Int -> [Int] -> Int
howManyAfter gens fish =
  length (iterate (concatMap spawnDay) fish !! gens)

-- for now do just a list, may need to graduate to fancier structure
spawnDay :: Int -> [Int]
spawnDay 0 = [6,8]
spawnDay n = [n-1]

doPart1 :: [Char] -> Int
doPart1 _ = 0
