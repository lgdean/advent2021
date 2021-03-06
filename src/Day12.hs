module Day12
    (
      doPart2,
      doPart1
    ) where

import Data.Char (isLower)
import Data.List.Split (splitOn)
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Debug.Trace (trace)


-- mercifully there are no big-to-big connections in the input; this simplifies the search.
doPart1 :: [Char] -> Int
doPart1 input =
  let segments = map parseSegment $ lines input
      caveMap = buildCaveMap segments
      paths = part1PathsFrom "start" [] caveMap
  in length paths

parseSegment :: String -> (String, String)
parseSegment line =
  case splitOn "-" line of
    [one, other] -> (one, other)

buildCaveMap :: [(String, String)] -> Map String (Set.Set String)
buildCaveMap segments =
  let addSegment caveMap (a,b) = addOneDirection a b $ addOneDirection b a caveMap
  in foldl addSegment Map.empty segments

addOneDirection :: String -> String -> Map String (Set.Set String) -> Map String (Set.Set String)
addOneDirection from to = Map.insertWith Set.union from (Set.singleton to)

isSmall :: String -> Bool
isSmall = any isLower

part1PathsFrom :: String -> [String] -> Map String (Set.Set String) -> [[String]]
part1PathsFrom "end" pathSoFar _ = ["end" : pathSoFar]
part1PathsFrom cave pathSoFar caveMap
  | isSmall cave && cave `elem` pathSoFar = []
  | otherwise = concatMap (\x -> part1PathsFrom x (cave : pathSoFar) caveMap) (caveMap Map.! cave)

part2PathsFrom :: String -> [String] -> Map String (Set.Set String) -> [[String]]
part2PathsFrom "end" pathSoFar _ = ["end" : pathSoFar]
part2PathsFrom cave pathSoFar caveMap
  -- after we visit a single small cave a second time, the remaining search is the same as part 1
  | isSmall cave && cave `elem` pathSoFar = concatMap (\x -> part1PathsFrom x (cave : pathSoFar) caveMap) (caveMap Map.! cave)
  | otherwise                             = concatMap (\x -> part2PathsFrom x (cave : pathSoFar) caveMap) (caveMap Map.! cave)

doPart2 :: [Char] -> Int
doPart2 input =
  let segments = map parseSegment $ lines input
      caveMap = buildCaveMap segments
      restOfCaveMap = Map.insert "start" Set.empty caveMap
      paths = concatMap (\x -> part2PathsFrom x ["start"] restOfCaveMap) (caveMap Map.! "start")
  in length paths
