module Lib
    ( readIntLines
    , fixedPoint
    ) where

import Debug.Trace (trace)

readIntLines :: [Char] -> [Int]
readIntLines = map read . lines

fixedPoint :: Eq a => (a -> a) -> a -> a
fixedPoint f initState =
  let nextState = f initState
  in if initState == nextState then nextState else fixedPoint f nextState
