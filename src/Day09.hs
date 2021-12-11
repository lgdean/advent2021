module Day09
    (
      doPart2,
      doPart1
    ) where

import Data.Char (digitToInt)
import Data.List (sortOn)
import Data.Maybe (maybeToList)
import Data.Ord (Down(..))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Lib

import Debug.Trace (trace)

isLowPoint :: (Int, Int) -> Map.Map (Int, Int) Int -> Bool
isLowPoint (x,y) grid =
  let others = neighbors (x,y) grid
      value = grid Map.! (x,y)
  in all (value <) others

neighbors :: (Int, Int) -> Map.Map (Int, Int) a -> [a]
neighbors (x,y) grid =
  let coords = neighborCoords (x,y)
  in concatMap (\c -> maybeToList (Map.lookup c grid)) coords

neighborCoords :: (Int, Int) -> [(Int, Int)]
neighborCoords (x,y) = [ (x+a,y+b) | a <- [-1,0,1], b <- [-1,0,1], (a==0) /= (b==0) ]

doPart1 :: [Char] -> Int
doPart1 input =
  let grid = parseLayout input
      lowPointLocations = filter (`isLowPoint` grid) (Map.keys grid)
  in sum $ map ((+1) . (grid Map.!)) lowPointLocations

expandBasin :: Set.Set (Int, Int) -> Map.Map (Int, Int) Int -> Set.Set (Int, Int)
expandBasin basin grid =
  -- the text doesn't say explicitly, but they always just build up to 9. so:
  let basinPlus = Set.foldl Set.union basin $ Set.map (Set.fromList . neighborCoords) basin
  in Set.filter (\c -> Map.findWithDefault 9 c grid < 9) basinPlus

doPart2 :: [Char] -> Int
doPart2 input =
  let grid = parseLayout input
      lowPointLocations = filter (`isLowPoint` grid) (Map.keys grid)
      allBasins = map (fixedPoint (`expandBasin` grid) . Set.singleton) lowPointLocations
  in product $ take 3 $ sortOn Down $ map Set.size allBasins

parseLayout :: String -> Map.Map (Int, Int) Int
parseLayout input =
  let rows = lines input
  in Map.unions $ zipWith parseRow [0..] rows

parseRow :: Int -> String -> Map.Map (Int, Int) Int
parseRow n row =
  let coords = zip [0..] (repeat n)
  in Map.fromList $ zip coords (map digitToInt row)
