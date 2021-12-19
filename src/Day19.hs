module Day19
    (
--      doPart2,
      doPart1
    ) where

import Data.List.Split (splitOn)

import Debug.Trace (trace)


doPart1 :: [[Char]] -> Int
doPart1 inputs =
  let scannerResults = map (map parseBeacon . lines) inputs
      (x,y,z) = head $ head scannerResults
  in x+y+z

parseBeacon :: String -> (Int, Int, Int)
parseBeacon line =
  case splitOn "," line of
    [x,y,z] -> (read x, read y, read z)
    _       -> error $ "cannot parse: " ++ line
