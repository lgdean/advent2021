module Day02
    (
      positionProduct,
--      doPart2,
      doPart1
    ) where

import Data.List.Split(splitOn)

import Debug.Trace (trace)

positionProduct :: [String] -> Int
positionProduct input =
  let pairs = map parseLine input
      horizontalPos = sum $ map snd $ filter ((=="forward").fst) pairs
      ups = sum $ map snd $ filter ((=="up").fst) pairs
      downs = sum $ map snd $ filter ((=="down").fst) pairs
      depth = downs - ups
  in horizontalPos * depth

doPart1 :: [Char] -> Int
doPart1 input =
  positionProduct $ lines input

parseLine :: String -> (String,Int)
parseLine line =
  case splitOn " " line of
    [d, n] -> (d, read n)
