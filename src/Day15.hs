module Day15
    (
      doPart2,
      doPart1
    ) where

import Data.Char (digitToInt, intToDigit)
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromJust, maybeToList)

import Debug.Trace (trace)

import Lib


doPart1 :: [Char] -> Int
doPart1 input =
  let riskGrid = parseGrid input
      (destination@(maxX,maxY),destRisk) = Map.findMax riskGrid
--      endState = fixedPoint (exploreFromEnd riskGrid) $ Map.insert destination (Just destRisk) $ Map.map (const Nothing) riskGrid
      diagState = exploreFromEnd (maxX+maxY-1) riskGrid $ Map.insert destination (Just destRisk) Map.empty
      endState = fixedPoint (improvePaths riskGrid) $ Map.map fromJust diagState
  in endState Map.! (0,0) - riskGrid Map.! (0,0)
--  in trace (show endState) $ fromJust (endState Map.! (0,0)) - riskGrid Map.! (0,0)
--  in fromJust (endState Map.! destination)

improvePaths :: Map.Map (Int, Int) Int -> Map.Map (Int, Int) Int -> Map.Map (Int, Int) Int
improvePaths risks lowestSoFar =
  let shiftedRight = Map.mapKeys (\(x,y) -> (x+1,y)) lowestSoFar
      shiftedLeft = Map.mapKeys (\(x,y) -> (x-1,y)) lowestSoFar
      shiftedUp = Map.mapKeys (\(x,y) -> (x,y-1)) lowestSoFar
      shiftedDown = Map.mapKeys (\(x,y) -> (x,y+1)) lowestSoFar
      allShifts = map (Map.intersectionWith (+) risks) [shiftedRight, shiftedLeft, shiftedUp, shiftedDown]
      newLowest = Map.unionsWith min (lowestSoFar : allShifts)
  in trace "hello" newLowest

exploreFromEnd :: Int -> Map.Map (Int, Int) Int -> Map.Map (Int, Int) (Maybe Int) -> Map.Map (Int, Int) (Maybe Int)
exploreFromEnd (-1) _ lowestSoFar = lowestSoFar
exploreFromEnd d risks lowestSoFar =
  let newGeneration = Map.filterWithKey (\(x,y) _ -> x+y == d) risks
      newRisk (p, ownRisk) = fancyMin (map (maybePlus ownRisk) (neighbors p lowestSoFar))
  in exploreFromEnd (d-1) risks $ Map.union (Map.mapWithKey (curry newRisk) newGeneration) lowestSoFar

-- would be good to remember how to use Maybe monad
fancyMin :: [Maybe Int] -> Maybe Int
fancyMin xs =
  case catMaybes xs of
    [] -> Nothing
    vals -> Just (minimum vals)

maybePlus :: Int -> Maybe Int -> Maybe Int
maybePlus a (Just b) = Just (a+b)
maybePlus _ Nothing = Nothing

isNeighbor (x1, y1) (x2, y2) =
  abs diffX == 1 && diffY == 0 || diffX == 0 && abs diffY == 1
  where diffX = x1-x2
        diffY = y1-y2

neighbors :: (Int, Int) -> Map.Map (Int, Int) a -> [a]
neighbors (x,y) grid =
  let coords = neighborCoords (x,y)
  in concatMap (\c -> maybeToList (Map.lookup c grid)) coords

neighborCoords :: (Int, Int) -> [(Int, Int)]
neighborCoords (x,y) = [ (x+a,y+b) | a <- [-1,0,1], b <- [-1,0,1], (a==0) /= (b==0) ]

showGrid :: Map.Map (Int, Int) Int -> String
showGrid grid =
  let ((minX, minY), _) = Map.findMin grid
      ((maxX, maxY), _) = Map.findMax grid
      showRow r = [intToDigit $ grid Map.! (x,r) | x <- [minX..maxX]] ++ "\n"
      rows = [showRow y | y <- [minY..maxY]]
  in concat rows

parseGrid :: String -> Map.Map (Int, Int) Int
parseGrid input =
  let rows = lines input
  in Map.unions $ zipWith parseRow [0..] rows

parseRow :: Int -> String -> Map.Map (Int, Int) Int
parseRow n row =
  let coords = zip [0..] (repeat n)
  in Map.fromList $ zip coords (map digitToInt row)


doPart2 :: [Char] -> Int
doPart2 input =
  let partialGrid = parseGrid input
      (tileSize@(tileX,tileY),_) = Map.findMax partialGrid
      copyRight tile = Map.union tile $ Map.mapKeys (\(x,y) -> (x+tileX+1,y)) $ Map.map incrementRisk tile
      copyDown tile = Map.union tile $ Map.mapKeys (\(x,y) -> (x,y+tileY+1)) $ Map.map incrementRisk tile
      fullWidth = iterate copyRight partialGrid !! 4
      riskGrid = iterate copyDown fullWidth !! 4
      (destination@(maxX,maxY),destRisk) = Map.findMax riskGrid
      diagState = exploreFromEnd (maxX+maxY-1) riskGrid $ Map.insert destination (Just destRisk) Map.empty
      endState = fixedPoint (improvePaths riskGrid) $ Map.map fromJust diagState
  in endState Map.! (0,0) - riskGrid Map.! (0,0)

incrementRisk 9 = 1
incrementRisk n = n + 1
