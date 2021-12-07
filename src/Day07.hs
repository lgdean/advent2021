module Day07
    (
--      doPart2,
      leastFuel,
      doPart1
    ) where

import Debug.Trace (trace)

leastFuel :: [Int] -> Int
leastFuel positions =
  let maxPos = maximum positions
  in minimum $ map (`fuelTotal` positions) [0..maxPos]

fuelTotal dest positions = sum $ map (calcFuel dest) positions
calcFuel dest pos = abs (pos - dest)

doPart1 :: [Char] -> Int
doPart1 _ = 0
