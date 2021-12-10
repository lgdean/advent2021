module Day09
    (
--      doPart2,
      doPart1
    ) where

import Data.Char (digitToInt)
import Data.Maybe (maybeToList)
import qualified Data.Map.Strict as Map

import Debug.Trace (trace)

isLowPoint :: (Int, Int) -> Map.Map (Int, Int) Int -> Bool
isLowPoint (x,y) grid =
  let others = neighbors (x,y) grid
      value = grid Map.! (x,y)
  in all (value <) others

neighbors :: (Int, Int) -> Map.Map (Int, Int) a -> [a]
neighbors (x,y) grid =
  let coords = [ (x+a,y+b) | a <- [-1,0,1], b <- [-1,0,1], (a==0) /= (b==0) ]
  in concatMap (\c -> maybeToList (Map.lookup c grid)) coords

doPart1 :: [Char] -> Int
doPart1 input =
  let grid = parseLayout input
      lowPointLocations = filter (`isLowPoint` grid) (Map.keys grid)
  in sum $ map ((+1) . (grid Map.!)) lowPointLocations

parseLayout :: String -> Map.Map (Int, Int) Int
parseLayout input =
  let rows = lines input
  in Map.unions $ zipWith parseRow [0..] rows

parseRow :: Int -> String -> Map.Map (Int, Int) Int
parseRow n row =
  let coords = zip (repeat n) [0..]
  in Map.fromList $ zip coords (map digitToInt row)
