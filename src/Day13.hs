module Day13
    (
--      doPart2,
      doPart1
    ) where

import Data.List.Split (splitOn)
import Data.Map (Map)
import qualified Data.Map.Strict as Map

import Debug.Trace (trace)


doPart1 :: Int -> [Char] -> Int
doPart1 x input =
  let dots = map parsePoint $ lines input :: [(Int, Int)]
      paper = foldl (\m p -> Map.insert p True m) Map.empty dots :: Map (Int, Int) Bool
      folded = foldUp x paper
  in Map.size folded

parsePoint :: String -> (Int, Int)
parsePoint line =
  case splitOn "," line of
    [one, other] -> (read one, read other)

foldUp :: Int -> Map (Int, Int) Bool -> Map (Int, Int) Bool
foldUp x paper =
  -- assuming the easy case: bottom height always <= top height; no dots on the fold line
  let (top, bottom) = Map.partitionWithKey (\(a,_) _ -> a < x) paper
      flippedBottom = Map.mapKeys (\(a,b) -> (2*x-a, b)) bottom
      overlapped = Map.union top flippedBottom
  in overlapped
