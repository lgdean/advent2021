module Day18
    (
--      doPart2,
      doPart1
    ) where


import Debug.Trace (trace)

import Lib

data SnailfishNum = Regular Int | Pair SnailfishNum SnailfishNum

doPart1 :: [Char] -> Int
doPart1 input =
  0
