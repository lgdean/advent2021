module Day14
    (
      doPart2,
      readPaper,
      doPart1
    ) where

import Data.List.Split (splitOn)
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T

import Debug.Trace (trace)


readPaper :: [Char] -> Map (Char, Char) Char
readPaper input =
  let dots = map parsePoint $ lines input
      paper = Map.fromList dots
  in paper

betterMap :: Map (Char, Char) Char -> Map (Char, Char) [(Char, Char)]
betterMap = Map.mapWithKey (\(a,b) c -> [(a,c),(c,b)])

doPart1 :: String -> [Char] -> Int
doPart1 x input =
  let rules = betterMap $ readPaper input
      startLetter = head x
      initialCounts = zip (zip x $ tail x) (repeat 1)
      endPairs = iterate (oneStep rules) initialCounts !! 10
      secondCounts = Map.fromListWith (+) $ map (\((_,b),c) -> (b,c)) endPairs
      actualCounts = Map.adjust (+ 1) startLetter secondCounts
  in maximum (Map.elems actualCounts) - minimum (Map.elems actualCounts)

oneStep :: Map (Char, Char) [(Char, Char)] -> [((Char, Char), Int)] -> [((Char, Char), Int)]
oneStep fancyRules pairs =
  let expand (p, count) = map (\r -> (r, count)) (fancyRules Map.! p)
      allExpanded = concatMap expand pairs
      bigMap = Map.fromListWith (+) allExpanded
  in Map.toList bigMap

parsePoint :: String -> ((Char, Char), Char)
parsePoint line =
  case map strip $ splitOn "->" line of
    [[a,b], other] -> ((a,b), head other)
    _              -> error ("could not parse: " ++ line)
strip  = T.unpack . T.strip . T.pack


doPart2 :: String -> [Char] -> Int
doPart2 x input =
  let rules = readPaper input
      startLetter = head x
      fancyRules = betterMap rules
      initialCounts = zip (zip x $ tail x) (repeat 1)
      endPairs = iterate (oneStep fancyRules) initialCounts !! 40
      secondCounts = Map.fromListWith (+) $ map (\((_,b),c) -> (b,c)) endPairs
      actualCounts = Map.adjust (+ 1) startLetter secondCounts
  in maximum (Map.elems actualCounts) - minimum (Map.elems actualCounts)
