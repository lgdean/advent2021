module Lib
    ( readIntLines
    ) where

import Debug.Trace (trace)

readIntLines :: [Char] -> [Int]
readIntLines = map read . lines
