module Day05
    (
--      doPart2,
      doPart1
    ) where

import Data.List (group, sort)
import Data.List.Split (splitOn)

import Debug.Trace (trace)


type Line = ((Int, Int), (Int, Int))

allPointsInLine :: Line -> [(Int, Int)]
allPointsInLine (p1@(x1,y1),p2@(x2,y2))
  | p1 == p2 = [p1] -- never happens, but it feels nice to check
  | otherwise = zip (handyRange x1 x2) (handyRange y1 y2)

handyRange :: Int -> Int -> [Int]
handyRange a b
  | a == b    = repeat a
  | a < b     = [a..b]
  | otherwise = reverse [b..a]

parseLine :: String -> Line
parseLine str =
  case words str of
    [start, "->", end] -> (parsePoint start, parsePoint end)
    _ -> error "oops parse error"

parsePoint :: String -> (Int, Int)
parsePoint str =
  let (x:y:_) = splitOn "," str
  in (read x, read y)

doPart1 :: [Char] -> Int
doPart1 input =
  let theLines = map parseLine $ lines input
      thePoints = concatMap allPointsInLine theLines
      groupedPoints = group $ sort thePoints
  in length $ filter ((>1) . length) groupedPoints
