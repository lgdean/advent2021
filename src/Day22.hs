module Day22
    (
--      doPart2,
      doPart1
    ) where

import Data.List.Split (splitOn)

import Debug.Trace (trace)


doPart1 :: [Char] -> Int
doPart1 input =
  let rebootSteps = map parseLine $ lines input
      reversedSteps = reverse ((False, ((-50,-50,-50), (50,50,50))): rebootSteps)
      isOn cube = fst $ head $ filter (\(_,r) -> inRange r cube) reversedSteps
      cubesToTest = [(x,y,z) | x <- [-50..50], y <- [-50..50], z <- [-50..50]]
      allOnInTest = filter isOn cubesToTest
  in length allOnInTest

parseLine :: String -> (Bool, ((Int,Int,Int),(Int,Int,Int)))
parseLine line =
  let [onOrOff, ranges] = words line
      isOn = onOrOff == "on"
      [(xa,xb), (ya,yb), (za,zb)] = map (parseCoordRange . drop 2) $ splitOn "," ranges
  in (isOn, ((xa,ya,za),(xb,yb,zb)))

parseCoordRange :: String -> (Int, Int)
parseCoordRange r =
  let parts = splitOn ".." r
  in case parts of
    [a,b] -> (read a, read b)
    _     -> error ("cannot parse range " ++ r)

inRange :: ((Int,Int,Int),(Int,Int,Int)) -> (Int,Int,Int) -> Bool
inRange ((minx,miny,minz),(maxx,maxy,maxz)) (x,y,z) =
  minx <= x && x <= maxx && miny <= y && y <= maxy && minz <= z && z <= maxz
