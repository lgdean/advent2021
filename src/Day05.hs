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
allPointsInLine line@((x1,y1),(x2,y2))
  | x1 == x2 = zip (repeat x1) [(min y1 y2) .. (max y1 y2)]
  | y1 == y2 = zip [(min x1 x2) .. (max x1 x2)] (repeat y1)
  | x1 < x2 && y1 < y2 = zip [x1..x2] [y1..y2]
  | x1 < x2  = zip [x1..x2] $ reverse [y2..y1]
  | y1 < y2 = zip (reverse [x2..x1]) [y1..y2]
  | otherwise = zip (reverse [x2..x1]) (reverse [y2..y1])

isHorizontalOrVertical :: Line -> Bool
isHorizontalOrVertical ((x1,y1),(x2,y2)) = x1==x2 || y1==y2

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
