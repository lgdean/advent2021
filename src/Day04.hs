module Day04
    (
      winning,
--      doPart2,
      doPart1
    ) where

import Data.List.Split (splitOn)

import Debug.Trace (trace)


type Board = [[(Bool, Int)]]

winning :: Board -> Bool
winning board = rowAllMarked board || columnAllMarked board;

rowAllMarked :: Board -> Bool
rowAllMarked board = any (all fst) board
columnAllMarked :: Board -> Bool
columnAllMarked board = any (all fst) $ transpose board

sumUnmarked :: Board -> Int
sumUnmarked board = sum $ map snd $ filter (not . fst) $ concat board

callNumber :: Int -> Board -> Board
callNumber n board =
  map (map updateIfCalled) board
  where updateIfCalled (curr, x) = (curr || x==n, x)

parseBoardList :: String -> [Board]
parseBoardList input =
  let allLines = lines input
      chunks = splitOn [""] allLines
  in map parseBoard chunks

parseBoard :: [String] -> Board
parseBoard rows =
  let boardNum str = (False, read str :: Int)
  in map (map boardNum . words) rows

playGame :: [Int] -> [Board] -> Int
playGame [] _ = error "ran out of numbers called!"
playGame (curr:rest) boards =
  let nextState = map (callNumber curr) boards
      winners = filter winning nextState
  in case winners of
    (aWinner:_) -> curr * sumUnmarked aWinner
    []          -> playGame rest nextState

doPart1 :: [Int] -> [Char] -> Int
doPart1 nums input = playGame nums $ parseBoardList input

-- copied from internet, I assume there is an appropriate library
transpose:: [[a]]->[[a]]
transpose ([]:_) = []
transpose x = map head x : transpose (map tail x)
