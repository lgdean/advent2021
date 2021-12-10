module Day10
    (
      doPart2,
      doPart1
    ) where

import Data.List (sort)

import Debug.Trace (trace)

doPart2 :: [Char] -> Int
doPart2 input =
  let relevantLines = filter (not . isCorrupted) (lines input)
      scores = map (part2ScoreLine . expectedCompletion []) relevantLines
  in sort scores !! (length scores `div` 2)

part2ScoreLine :: String -> Int
part2ScoreLine str = foldl (\acc n -> acc*5+n) 0 (map part2Score str)

isCorrupted :: String -> Bool
isCorrupted line = scoreLine line > 0

expectedCompletion :: [Char] -> [Char] -> [Char]
expectedCompletion stack [] = stack
expectedCompletion [] (')':_) = error "extra closer"
expectedCompletion [] (']':_) = error "extra closer"
expectedCompletion [] ('}':_) = error "extra closer"
expectedCompletion [] ('>':_) = error "extra closer"
expectedCompletion stack (x:xs)
  | x `elem` ['(', '[', '{', '<'] = expectedCompletion (closerFor x : stack) xs
  | x `elem` [')', ']', '}', '>'] && x == head stack = expectedCompletion (tail stack) xs
  | x `elem` [')', ']', '}', '>'] && x /= head stack = error "corrupted line"

part2Score :: Char -> Int
part2Score ')' = 1
part2Score ']' = 2
part2Score '}' = 3
part2Score '>' = 4
part2Score x = error ("cannot score unexpected char " ++ [x])

doPart1 :: [Char] -> Int
doPart1 input =
  let navLines = lines input
      scores = map scoreLine navLines
  in sum scores

scoreLine :: [Char] -> Int
scoreLine = scoreRest []

scoreRest :: [Char] -> [Char] -> Int
scoreRest _ [] = 0
scoreRest [] (')':_) = 3
scoreRest [] (']':_) = 57
scoreRest [] ('}':_) = 1197
scoreRest [] ('>':_) = 25137
scoreRest stack (x:xs)
  | x `elem` ['(', '[', '{', '<'] = scoreRest (closerFor x : stack) xs
  | x `elem` [')', ']', '}', '>'] && x == head stack = scoreRest (tail stack) xs
  | x `elem` [')', ']', '}', '>'] && x /= head stack = charScore x

charScore :: Char -> Int
charScore ')' = 3
charScore ']' = 57
charScore '}' = 1197
charScore '>' = 25137

closerFor '(' = ')'
closerFor '[' = ']'
closerFor '{' = '}'
closerFor '<' = '>'
