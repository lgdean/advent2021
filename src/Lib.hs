module Lib
    ( readIntLines
    , fixedPoint
    , bin2Int
    ) where

import Data.Char(digitToInt)

import Debug.Trace (trace)

readIntLines :: [Char] -> [Int]
readIntLines = map read . lines

fixedPoint :: Eq a => (a -> a) -> a -> a
fixedPoint f initState =
  let nextState = f initState
  in if initState == nextState then nextState else fixedPoint f nextState

bin2Int :: String -> Int
bin2Int str = foldl (\acc n -> acc*2+n) 0 (map digitToInt str)
