module Day02
    (
      positionProduct,
      part2Product,
      doPart2,
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

part2Product :: [String] -> Int
part2Product input =
 let pairs = map parseLine input
     result = foldl navigateStep (0,0,0) pairs
     (x, depth, _) = result
 in x*depth

navigateStep :: (Int, Int, Int) -> (String, Int) -> (Int, Int, Int)
navigateStep (x, depth, aim) ("forward", dx) = (x+dx, depth+aim*dx, aim)
navigateStep (x, depth, aim) ("up", daim) = (x, depth, aim-daim)
navigateStep (x, depth, aim) ("down", daim) = (x, depth, aim+daim)

doPart2 :: [Char] -> Int
doPart2 input =
  part2Product $ lines input

parseLine :: String -> (String,Int)
parseLine line =
  case splitOn " " line of
    [d, n] -> (d, read n)
