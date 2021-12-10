module Day03
    (
      gammaRate,
      part1answer,
      part2answer,
      oxygenGeneratorRating,
      doPart2,
      doPart1
    ) where

import Data.List(groupBy, maximumBy, sort, sortBy, sortOn, transpose)
import Data.Ord(comparing)

import Debug.Trace (trace)

import Lib

gammaRate :: [String] -> Int
gammaRate input =
  let columns = transpose input
      gamma = map moreCommonBit columns
  in bin2Int gamma

oppositeBit '0' = '1'
oppositeBit '1' = '0'

moreCommonBit :: String -> Char
moreCommonBit input =
  let ones = length $ filter (=='1') input
      zeroes = length input - ones
  in if ones > zeroes then '1' else '0' -- silly but it works

part1answer :: [String] -> Int
part1answer input =
  let columns = transpose input
      gamma = map moreCommonBit columns
      epsilon = map oppositeBit gamma
  in bin2Int gamma * bin2Int epsilon


doPart1 :: [Char] -> Int
doPart1 input =
  part1answer $ lines input

oxygenGeneratorRating :: [String] -> String
oxygenGeneratorRating [] = ""
oxygenGeneratorRating input
  | all null input = ""
  | length input == 1 = head input
  | otherwise =
  let sorted = sort input
      grouped = groupBy (\x y -> head x == head y) sorted
      common = maximumBy (comparing length) grouped
  in head (head common) : oxygenGeneratorRating (map tail common)

co2scrubberRating :: [String] -> String
co2scrubberRating [] = ""
co2scrubberRating input
  | all null input = ""
  | length input == 1 = head input
  | otherwise =
  let sorted = sort input
      grouped = groupBy (\x y -> head x == head y) sorted
      common = maximumBy (comparing (negate . length)) $ reverse grouped -- goofy way to prefer zero
  in head (head common) : co2scrubberRating (map tail common)

part2answer :: [String] -> Int
part2answer input =
  bin2Int (oxygenGeneratorRating input) * bin2Int (co2scrubberRating input)

doPart2 :: [Char] -> Int
doPart2 input =
  part2answer $ lines input
