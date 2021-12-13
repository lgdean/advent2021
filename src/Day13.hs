module Day13
    (
--      doPart2,
      foldLeft,
      foldUp,
      readPaper,
      doPart1Left,
      doPart1Up
    ) where

import Data.List.Split (splitOn)
import Data.Map (Map)
import qualified Data.Map.Strict as Map

import Debug.Trace (trace)


readPaper :: [Char] -> Map (Int, Int) Bool
readPaper input =
  let dots = map parsePoint $ lines input :: [(Int, Int)]
      paper = foldl (\m p -> Map.insert p True m) Map.empty dots :: Map (Int, Int) Bool
  in paper

doPart1Left :: Int -> [Char] -> Int
doPart1Left x input =
  let paper = readPaper input
      folded = foldLeft x paper
  in Map.size folded

doPart1Up :: Int -> [Char] -> Int
doPart1Up y input =
  let dots = map parsePoint $ lines input :: [(Int, Int)]
      paper = foldl (\m p -> Map.insert p True m) Map.empty dots :: Map (Int, Int) Bool
      folded = foldUp y paper
  in Map.size folded

parsePoint :: String -> (Int, Int)
parsePoint line =
  case splitOn "," line of
    [one, other] -> (read one, read other)

foldUp :: Int -> Map (Int, Int) Bool -> Map (Int, Int) Bool
foldUp y paper =
  -- assuming the easy case: bottom height always <= top height; no dots on the fold line
  let (top, bottom) = Map.partitionWithKey (\(a,b) _ -> b < y) paper
      flippedBottom = Map.mapKeys (\(a,b) -> (a, 2*y-b)) bottom
      overlapped = Map.union top flippedBottom
  in overlapped

foldLeft :: Int -> Map (Int, Int) Bool -> Map (Int, Int) Bool
foldLeft x paper =
  -- assuming the easy case: bottom height always <= top height; no dots on the fold line
  let (top, bottom) = Map.partitionWithKey (\(a,_) _ -> a < x) paper
      flippedBottom = Map.mapKeys (\(a,b) -> (2*x-a, b)) bottom
      overlapped = Map.union top flippedBottom
  in overlapped
