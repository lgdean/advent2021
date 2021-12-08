module Day08
    (
      decodeEntry,
      doPart2,
      doPart1
    ) where

import Data.List((\\), intersect, partition, sort)
import Data.List.Split(splitOn)

import Debug.Trace (trace)

doPart1 :: [Char] -> Int
doPart1 input =
  let entries = map parseLine $ lines input
      outputs = map snd entries
  in length $ concatMap (filter ((`elem` [2,3,4,7]) . length)) outputs

areAllIn :: Eq a => [a] -> [a] -> Bool
areAllIn candidates pool =
  all (`elem` pool) candidates

decodeEntry :: [String] -> String -> Int
decodeEntry notedPatterns readout =
  let patterns = map sort notedPatterns
      eightPattern = head $ filter ((==) 7 . length) patterns
      sevenPattern = head $ filter ((==) 3 . length) patterns
      onePattern = head $ filter ((==) 2 . length) patterns
      upperSegment = head $ sevenPattern \\ onePattern
      fourPattern = head $ filter ((==) 4 . length) patterns
      ([ninePattern], zeroAndSix) = partition (fourPattern `areAllIn`) $ filter ((==) 6 . length) patterns
      ([zeroPattern], [sixPattern]) = partition (onePattern `areAllIn`) zeroAndSix
      middleSegment = head $ eightPattern \\ zeroPattern
      upperLeftSegment = head $ fourPattern \\ (middleSegment : onePattern)
      twoThreeAndFive = filter ((==) 5 . length) patterns
      horizontalSegments = foldl intersect eightPattern twoThreeAndFive :: [Char]
      lowerSegment = head $ horizontalSegments \\ [upperSegment, middleSegment]
      threePattern = head $ filter (onePattern `areAllIn`) twoThreeAndFive
      ([fivePattern], [twoPattern]) = partition (upperLeftSegment `elem`) (twoThreeAndFive \\ [threePattern])
  in case sort readout of
    x | x == zeroPattern -> 0
    x | x == onePattern -> 1
    x | x == twoPattern -> 2
    x | x == threePattern -> 3
    x | x == fourPattern -> 4
    x | x == fivePattern -> 5
    x | x == sixPattern -> 6
    x | x == sevenPattern -> 7
    x | x == eightPattern -> 8
    x | x == ninePattern -> 9

decodeReadout :: [String] -> [String] -> Int
decodeReadout patterns outputs =
  let decoder = decodeEntry patterns
  in foldl (\acc n -> acc*10+n) 0 (map decoder outputs)

doPart2 :: [Char] -> Int
doPart2 input =
  let entries = map parseLine $ lines input
      decoded = map (uncurry decodeReadout) entries
  in sum decoded

parseLine :: String -> ([String], [String])
parseLine str =
  case splitOn ["|"] $ words str of
    [patterns, output] -> (patterns, output)
    _ -> error "oops parse error"
