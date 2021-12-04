module Day04
    (
      winning,
      doPart2,
      doPart1
    ) where

import Data.List (partition)
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

playGame :: [Int] -> [Board] -> (Int, Board)
playGame [] _ = error "ran out of numbers called!"
playGame (curr:rest) boards =
  let nextState = map (callNumber curr) boards
      winners = filter winning nextState
  in case winners of
    (aWinner:_) -> (curr, aWinner)
    []          -> playGame rest nextState

calcScore :: (Int, Board) -> Int
calcScore (n, board) = n * sumUnmarked board

doPart1 :: [Int] -> [Char] -> Int
doPart1 nums input = calcScore $ playGame nums $ parseBoardList input

pickLastWinner :: [Int] -> [Board] -> (Int, Board)
pickLastWinner [] _ = error "other guard should catch this case before we recurse here"
pickLastWinner (curr:rest) boards =
  let nextState = map (callNumber curr) boards
      (winners, losers) = partition winning nextState
  in case (winners, losers) of
    (aWinner:_, []) -> (curr, aWinner)
    (_        , []) -> error "oh no"
    (_        , _)  -> pickLastWinner rest losers

doPart2 :: [Int] -> [Char] -> Int
doPart2 nums input = calcScore $ pickLastWinner nums $ parseBoardList input

-- copied from internet, I assume there is an appropriate library
transpose:: [[a]]->[[a]]
transpose ([]:_) = []
transpose x = map head x : transpose (map tail x)
