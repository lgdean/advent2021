module Day19
    (
--      doPart2,
      findOverlapParsing,
      rotationsOld,
      doPart1test,
      doPart1FromBigFile,
      doPart1
    ) where

import Data.List (intercalate, intersect, maximumBy)
import Data.List.Split (splitOn)
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Ord (comparing)
import qualified Data.Set as Set
import Data.Set (Set)

import Debug.Trace (trace)


doPart1FromBigFile :: [Char] -> Int
doPart1FromBigFile fullInput =
  let allLines = lines fullInput
      chunks = splitOn [""] allLines
  in doPart1 $ map (intercalate "\n" . tail) chunks

doPart1 :: [[Char]] -> Int
doPart1 inputs =
  length $ doPart1test inputs

doPart1test :: [[Char]] -> [(Int, Int, Int)]
doPart1test inputs =
  let scannerResults = map (map parseBeacon . lines) inputs
      -- hoping scanner 0 will work this way; if not, fix code
      scanners = Map.fromList $ zip [0..] scannerResults
      referenceScanner = head scannerResults
      fullMap = fst $ buildBeaconMap (Set.fromList referenceScanner) referenceScanner (Map.delete 0 scanners)
  in Set.toList fullMap

buildBeaconMap :: Set (Int, Int, Int) -> [(Int, Int, Int)] -> Map Int [(Int, Int, Int)] -> (Set (Int, Int, Int), Map Int [(Int, Int, Int)])
buildBeaconMap soFar curr scanners
  | Map.null scanners = (soFar, scanners)
  | otherwise = let overlaps = Map.filter ((>= 12) . length . fst) $ Map.map (findOverlap curr) scanners
             in case Map.size overlaps of
               0 -> (soFar, scanners)
               1 -> buildBeaconMap (Set.union soFar $ Set.fromList rotated) rotated nextRest
                    where overlapKey = head $ Map.keys overlaps
                          (overlap, rotated) = overlaps Map.! overlapKey
                          nextRest = Map.delete overlapKey scanners
               _ -> let results = Map.mapWithKey(\k (o,r) -> buildBeaconMap (Set.fromList r) r (Map.delete k scanners)) overlaps
                        (rotatedSets, remainingMaps) = unzip $ Map.elems results
                        remainingScanners = foldl Map.intersection scanners remainingMaps
                    in (Set.unions (soFar : rotatedSets), remainingScanners)
--                      _ -> error "oh heck"
--               n -> error "oh no, shortcut did not work"

findOverlap :: [(Int, Int, Int)] -> [(Int, Int, Int)] -> ([(Int, Int, Int)], [(Int, Int, Int)])
findOverlap ref other =
  let rotatedOther = map (`map` other) rotations
      overlaps = map (maxOverlap ref) rotatedOther
      overlapsAndAdjustedOther = zipWith (\(ovlp, offset) rotated -> (ovlp, map (`distance3d` offset) rotated)) overlaps rotatedOther
  in maximumBy (comparing (length . fst)) overlapsAndAdjustedOther -- TODO if needed can find only first >= 12

maxOverlap :: [(Int, Int, Int)] -> [(Int, Int, Int)] -> ([(Int, Int, Int)], (Int, Int, Int))
maxOverlap ref other =
  let diffsFromRef = concat [map (`distance3d` r) other | r <- ref] :: [(Int, Int, Int)]
      overlaps = map (\d -> (overlapUsing d ref other, d)) diffsFromRef
      mostInCommon = maximumBy (comparing (length . fst)) overlaps
      -- TODO why isn't this filter just an optimization? hm.
--      mostInCommon = maximumBy (comparing (length . fst)) $ filter ((>=12) . length . fst) overlaps
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
