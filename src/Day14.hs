module Day14
    (
--      doPart2,
      readPaper,
      doPart1
    ) where

import Data.List (group, sort)
import Data.List.Split (splitOn)
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes)
import qualified Data.Text as T

import Debug.Trace (trace)


readPaper :: [Char] -> Map (Char, Char) Char
readPaper input =
  let dots = map parsePoint $ lines input
      paper = Map.fromList dots
  in paper

doPart1 :: String -> [Char] -> Int
doPart1 x input =
  let rules = readPaper input
      polymer = iterate (insertAll rules) x !! 10
      counts = map length $ group $ sort polymer
  in maximum counts - minimum counts

parsePoint :: String -> ((Char, Char), Char)
parsePoint line =
  case map strip $ splitOn "->" line of
    [[a,b], other] -> ((a,b), head other)
    _              -> error ("could not parse: " ++ line)
strip  = T.unpack . T.strip . T.pack

insertAll :: Map (Char, Char) Char -> String -> String
insertAll rules polymer =
  let pairs = zip polymer $ tail polymer
      newElements = map (`Map.lookup` rules) pairs
      almostResult = alternate (map Just polymer) newElements
  in catMaybes almostResult

alternate :: [a] -> [a] -> [a]
alternate (a:as) (b:bs) = a:b:alternate as bs
alternate [] bs = bs
alternate as [] = as

insertElement :: Maybe Char -> String -> String
insertElement (Just x) (a:bs) = a:x:bs
insertElement Nothing xs = xs
