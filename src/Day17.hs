module Day17
    (
--      doPart2,
      doPart1
    ) where

import Data.Range

import Debug.Trace (trace)


doPart1 :: Range Int -> Range Int -> Int
doPart1 xtarget ytarget =
  let plausibleDX = takeWhile (not . aboveRange xtarget) [1..]
      plausibleDY = [0..118]
      infinitePath (x,y) (dx,dy) = map fst $ iterate (uncurry doStep) ((x,y),(dx,dy)) :: [(Int, Int)]
      pathOf (x,y) (dx,dy) = takeWhile (not . pastTarget xtarget ytarget) $ infinitePath (x,y) (dx,dy)
      allGoodPaths = filter (any (inTarget xtarget ytarget)) [pathOf (0,0) (dx,dy) | dx <- plausibleDX, dy <- plausibleDY]
      maxYs = map (maximum . map snd) allGoodPaths :: [Int]
  in maximum maxYs

doStep (x,y) (dx,dy) =
  ((x+dx,y+dy), adjustVelocity (dx,dy))

-- not planning to have negative x velocity; can adjust this function if needed for Part 2
adjustVelocity (0,dy) = (0,dy-1)
adjustVelocity (x,dy) = (x-1,dy-1)

inTarget xtarget ytarget (x,y) =
  inRange xtarget x && inRange ytarget y

pastTarget xtarget ytarget (x,y) =
  aboveRange xtarget x || belowRange ytarget y
