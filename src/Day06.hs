module Day06
    (
--      doPart2,
      howManyAfter,
      doPart1
    ) where

import Data.Maybe (fromMaybe)
import Data.List (group, sort)

import Debug.Trace (trace)


howManyAfter :: Int -> [Int] -> Int
howManyAfter gens fish =
  let initCount = map (\xs -> (head xs, length xs)) $ group $ sort fish
  in countFish (iterate fancySpawn initCount !! gens)

countFish :: [(Int, Int)] -> Int
countFish fishCounts = sum $ map snd fishCounts

fancySpawn :: [(Int, Int)] -> [(Int, Int)]
fancySpawn fishCounts =
  let eights = lookupFish 0 fishCounts
      sixes = lookupFish 0 fishCounts + lookupFish 7 fishCounts
      sevens = lookupFish 8 fishCounts
  in (6, sixes) : (7, sevens) : (8, eights)
   : [(n, lookupFish (n+1) fishCounts) | n <- [0..5]]

lookupFish :: Int -> [(Int, Int)] -> Int
lookupFish fishKind fishCounts =
  fromMaybe 0 (lookup fishKind fishCounts)

doPart1 :: [Char] -> Int
doPart1 _ = 0
