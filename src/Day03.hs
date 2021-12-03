module Day03
    (
      gammaRate,
      part1answer,
--      doPart2,
      doPart1
    ) where

import Data.Char(digitToInt)

import Debug.Trace (trace)

gammaRate :: [String] -> Int
gammaRate input =
  let columns = transpose input
      gamma = map moreCommonBit columns
  in bin2Int gamma

bin2Int :: String -> Int
bin2Int str = foldl (\acc n -> acc*2+n) 0 (map digitToInt str)

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

-- copied from internet, I assume there is an appropriate library
transpose:: [[a]]->[[a]]
transpose ([]:_) = []
transpose x = map head x : transpose (map tail x)
