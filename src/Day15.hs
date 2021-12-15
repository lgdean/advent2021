module Day15
    (
--      doPart2,
      doPart1
    ) where

import Data.Char (digitToInt, intToDigit)
import Data.List.Split (splitOn)
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromJust)

import Debug.Trace (trace)

import Lib


doPart1 :: [Char] -> Int
doPart1 input =
  let riskGrid = parseGrid input
      (destination@(maxX,maxY),destRisk) = Map.findMax riskGrid
--      endState = fixedPoint (exploreFromEnd riskGrid) $ Map.insert destination (Just destRisk) $ Map.map (const Nothing) riskGrid
      diagState = exploreFromEnd (maxX+maxY-1) riskGrid $ Map.insert destination (Just destRisk) Map.empty
      endState = fixedPoint (improvePaths riskGrid) $ Map.map fromJust diagState
  in trace (show endState) $ endState Map.! (0,0) - riskGrid Map.! (0,0)
--  in trace (show endState) $ fromJust (endState Map.! (0,0)) - riskGrid Map.! (0,0)
--  in fromJust (endState Map.! destination)

improvePaths :: Map.Map (Int, Int) Int -> Map.Map (Int, Int) Int -> Map.Map (Int, Int) Int
improvePaths risks lowestSoFar =
  let neighbors p = Map.filterWithKey (\k _ -> isNeighbor p k) lowestSoFar
      newRisk (p, ownRisk) = minimum (lowestSoFar Map.! p : map (+ ownRisk) (Map.elems (neighbors p)))
  in Map.mapWithKey (curry newRisk) risks

exploreFromEnd :: Int -> Map.Map (Int, Int) Int -> Map.Map (Int, Int) (Maybe Int) -> Map.Map (Int, Int) (Maybe Int)
exploreFromEnd (-1) _ lowestSoFar = lowestSoFar
exploreFromEnd d risks lowestSoFar =
  let newGeneration = Map.filterWithKey (\(x,y) _ -> x+y == d) risks
      neighbors p = Map.filterWithKey (\k _ -> isNeighbor p k) lowestSoFar
      newRisk (p, ownRisk) = fancyMin (map (maybePlus ownRisk) (Map.elems (neighbors p)))
  in exploreFromEnd (d-1) risks $ Map.union (Map.mapWithKey (curry newRisk) newGeneration) lowestSoFar

--exploreFromEnd :: Map.Map (Int, Int) Int -> Map.Map (Int, Int) (Maybe Int) -> Map.Map (Int, Int) (Maybe Int)
--exploreFromEnd risks lowestSoFar =
--  let neighbors p = Map.filterWithKey (\k _ -> isNeighbor p k) lowestSoFar
--      newRisk (p, ownRisk) = fancyMin (lowestSoFar Map.! p : map (maybePlus ownRisk) (Map.elems (neighbors p)))
--  in Map.mapWithKey (curry newRisk) risks

-- would be good to remember how to use Maybe monad
fancyMin :: [Maybe Int] -> Maybe Int
fancyMin xs =
  case catMaybes xs of
    [] -> Nothing
    vals -> Just (minimum vals)

maybePlus :: Int -> Maybe Int -> Maybe Int
maybePlus a (Just b) = Just (a+b)
maybePlus _ Nothing = Nothing

lowestTotalRiskTo :: (Int, Int) -> (Int, Int) -> Map.Map (Int, Int) Int -> Int
lowestTotalRiskTo dest@(destX, destY) src@(srcX, srcY) grid
  | dest == src = 0
  | isNeighbor dest src = grid Map.! dest
  | otherwise = minimum $ map (\(p,r) -> r + lowestTotalRiskTo p src grid) (neighbors dest)
  where neighbors p = Map.toList $ Map.filterWithKey (\k _ -> isNeighbor p k) grid

isNeighbor (x1, y1) (x2, y2) =
  abs diffX == 1 && diffY == 0 || diffX == 0 && abs diffY == 1
  where diffX = x1-x2
        diffY = y1-y2

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
