module Day10
    (
--      doPart2,
      doPart1
    ) where

import Debug.Trace (trace)

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
