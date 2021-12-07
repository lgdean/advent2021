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
calcFuel dest pos =
  let distance = abs (pos - dest)
  in distance * (distance + 1) `div` 2

doPart1 :: [Char] -> Int
doPart1 _ = 0
