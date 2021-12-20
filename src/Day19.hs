module Day19
    (
--      doPart2,
      findOverlapParsing,
      rotationsOld,
      doPart1
    ) where

import Data.List (intersect, maximumBy)
import Data.List.Split (splitOn)
import Data.Ord (comparing)

import Debug.Trace (trace)


doPart1 :: [[Char]] -> Int
doPart1 inputs =
  let scannerResults = map (map parseBeacon . lines) inputs
      (x,y,z) = head $ head scannerResults
  in x+y+z

findOverlap :: [(Int, Int, Int)] -> [(Int, Int, Int)] -> ([(Int, Int, Int)], (Int, Int, Int))
findOverlap ref other =
  let rotatedOther = map (`map` other) rotations
      overlaps = map (maxOverlap ref) rotatedOther
  in maximumBy (comparing (length . fst)) overlaps

maxOverlap :: [(Int, Int, Int)] -> [(Int, Int, Int)] -> ([(Int, Int, Int)], (Int, Int, Int))
maxOverlap ref other =
  let diffsFromRef = concat [map (`distance3d` r) other | r <- ref] :: [(Int, Int, Int)]
      overlaps = map (\d -> (overlapUsing d ref other, d)) diffsFromRef
      mostInCommon = maximumBy (comparing (length . fst)) overlaps
  in mostInCommon

overlapUsing :: (Int, Int, Int) -> [(Int, Int, Int)] -> [(Int, Int, Int)] -> [(Int, Int, Int)]
overlapUsing diff refs others =
  let relativeOthers = map (`distance3d` diff) others
  in intersect refs relativeOthers

-- really a difference not a distance
distance3d (a,b,c) (x,y,z) = (a-x, b-y, c-z)

rotations :: [(Int, Int, Int) -> (Int, Int, Int)]
rotations =
  -- first treating z as a given "facing direction" and doing a cartwheel
  let rotateWithSameFacing = [id, \(x,y,z) -> (-y,x,z), \(x,y,z) -> (-x,-y,z), \(x,y,z) -> (y,-x,z)]
      -- then changing the facing direction: look [same], behind, up, down, left, right
      turnAroundOrNot = [id, \(x,y,z) -> (-x,y,-z)]
      lookSameUpOrRight = [id, \(x,y,z) -> (x,z,-y), \(x,y,z) -> (-z,y,x)]
  in [r . dir . axis | r <- rotateWithSameFacing, dir <- turnAroundOrNot, axis <- lookSameUpOrRight]

-- handy for testing
rotationsOld :: (Int, Int, Int) -> [(Int, Int, Int)]
rotationsOld p =
  -- first treating z as a given "facing direction" and doing a cartwheel
  let rotateWithSameFacing (x,y,z) = [(x,y,z), (-y,x,z), (-x,-y,z), (y,-x,z)]
      -- then changing the facing direction: look [same], behind, up, down, left, right
      turnAroundOrNot (x,y,z) = [(x,y,z), (-x,y,-z)]
      lookSameUpOrRight (x,y,z) = [(x,y,z), (x,z,-y), (-z,y,x)]
  in [dir | axis <- lookSameUpOrRight p, r <- rotateWithSameFacing axis, dir <- turnAroundOrNot r]

findOverlapParsing :: [Char] -> [Char] -> [(Int, Int, Int)]
findOverlapParsing scanner0 scanner1 =
  let beacons0 = map parseBeacon $ lines scanner0
      beacons1 = map parseBeacon $ lines scanner1
      (overlap, _) = findOverlap beacons0 beacons1
  in overlap

parseBeacon :: String -> (Int, Int, Int)
parseBeacon line =
  case splitOn "," line of
    [x,y,z] -> (read x, read y, read z)
    _       -> error $ "cannot parse: " ++ line
