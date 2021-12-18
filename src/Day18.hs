module Day18
    (
--      doPart2,
      parseNum,
      magnitude,
      doPart1
    ) where

import Data.Char (digitToInt, intToDigit)

import Debug.Trace (trace)

import Lib

data SnailfishNum = Regular Int | Pair SnailfishNum SnailfishNum deriving Show

doPart1 :: [Char] -> Int
doPart1 input =
  0

magnitude :: SnailfishNum -> Int
magnitude (Regular x) = x
magnitude (Pair x y) = 3 * magnitude x + 2 * magnitude y

-- this is where it would have been nice to learn Parsec
parseNum :: String -> SnailfishNum
parseNum input =
  case parseInner input of
    (result, [])  -> result
    (_, leftover) -> error ("oops unexpected remaining string: " ++ leftover)

parseInner :: String -> (SnailfishNum, String)
parseInner ('[':content) =
  let (firstResult,more) = parseInner content
      (comma:second) = more
      (secondResult,end) = parseInner second
      (closer:remainder) = end
  in case (comma, closer) of
    (',', ']') -> (Pair firstResult secondResult, remainder)
    _          -> error "mistake in parsing"
parseInner (x:rest) =
  (Regular (digitToInt x), rest)
parseInner "" = error "parseInner on empty string, no good"
