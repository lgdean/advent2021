module Day08
    (
--      doPart2,
      doPart1
    ) where

import Data.List.Split(splitOn)

import Debug.Trace (trace)

doPart1 :: [Char] -> Int
doPart1 input =
  let entries = map parseLine $ lines input
      outputs = map snd entries
  in length $ concatMap (filter ((`elem` [2,3,4,7]) . length)) outputs

parseLine :: String -> ([String], [String])
parseLine str =
  case splitOn ["|"] $ words str of
    [patterns, output] -> (patterns, output)
    _ -> error "oops parse error"
