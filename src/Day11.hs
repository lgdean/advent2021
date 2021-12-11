module Day11
    (
      doPart2,
      doPart1
    ) where

import Data.Char (digitToInt, intToDigit)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Debug.Trace (trace)


doPart2 :: [Char] -> Int
doPart2 input =
  let grid = parseGrid input
      startAndResults = iterate doStep (0, grid)
  in length $ takeWhile ((<100) . fst) startAndResults

doPart1 :: [Char] -> Int
doPart1 input =
  let grid = parseGrid input
      startAndResults = take 101 $ iterate doStep (0, grid)
      flashCounts = map fst startAndResults
  in sum $ trace (showGrid grid) flashCounts

showGrid :: Map.Map (Int, Int) Int -> String
showGrid grid =
  let showRow r = [intToDigit $ grid Map.! (x,r) | x <- [0..9]] ++ "\n"
      rows = [showRow y | y <- [0..9]]
  in concat rows

parseGrid :: String -> Map.Map (Int, Int) Int
parseGrid input =
  let rows = lines input
  in Map.unions $ zipWith parseRow [0..] rows

parseRow :: Int -> String -> Map.Map (Int, Int) Int
parseRow n row =
  let coords = zip [0..] (repeat n)
  in Map.fromList $ zip coords (map digitToInt row)

neighborCoords :: (Int, Int) -> [(Int, Int)]
neighborCoords (x,y) = [ (x+a,y+b) | a <- [-1,0,1], b <- [-1,0,1], not ((a==0) && (b==0)) ]

doStep :: (a, Map.Map (Int, Int) Int) -> (Int, Map.Map (Int, Int) Int)
doStep (_, grid) =
  let firstPart = initStep grid
      nextPart = doFlashing firstPart
      howManyFlashed = Map.size $ Map.filter snd nextPart
      newState = Map.map resetEnergy nextPart
  in (howManyFlashed, newState)

-- increment the count and note it has not flashed yet
initStep :: Map.Map (Int, Int) Int -> Map.Map (Int, Int) (Int, Bool)
initStep = Map.map (\x -> (x+1, False))

doFlashing :: Map.Map (Int, Int) (Int, Bool) -> Map.Map (Int, Int) (Int, Bool)
doFlashing octopi =
  let flasherCoords = locateFlashers octopi
      allNeigbors = concatMap neighborCoords flasherCoords
      updatedFlashers = Map.map (\(x,False) -> (x,True)) $ Map.restrictKeys octopi $ Set.fromList flasherCoords
      updatedNeighbors = foldl (flip (Map.adjust energizeNeighbor)) octopi allNeigbors
      result = Map.unions [updatedFlashers, updatedNeighbors, octopi]
  in
     if null flasherCoords
       then octopi
       else doFlashing result

-- don't bother increasing energy if it already flashed; hope this doesn't bite me in part 2
energizeNeighbor :: (Int, Bool) -> (Int, Bool)
energizeNeighbor (x, False) = (x+1, False)
energizeNeighbor n = n

locateFlashers :: Map.Map (Int, Int) (Int, Bool) -> [(Int, Int)]
locateFlashers octopi =
  Map.keys $ Map.filter (\(energy, hasFlashed) -> energy > 9 && not hasFlashed) octopi

resetEnergy :: (Int, Bool) -> Int
resetEnergy (x, False) = x
resetEnergy (_, True) = 0
