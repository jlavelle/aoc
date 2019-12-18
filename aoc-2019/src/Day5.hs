module Day5 where

import IntCode (interpretOut, parseProgram)

solve1 :: [Int] -> [Int]
solve1 = flip interpretOut [1]

solve2 :: [Int] -> [Int]
solve2 = flip interpretOut [5]

solutions :: IO ()
solutions = do
  i <- parseProgram "inputs/day5"
  print $ solve1 i
  print $ solve2 i
