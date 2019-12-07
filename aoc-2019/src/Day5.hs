module Day5 where

import IntCode (interpretP, Program, parseProgram)

diagnostic :: [Int] -> Program -> Either String [Int]
diagnostic i p = fmap fst $ interpretP p i

solve1 :: Program -> Either String [Int]
solve1 = diagnostic [1]

solve2 :: Program -> Either String [Int]
solve2 = diagnostic [5]

solutions :: IO ()
solutions = do
  i <- parseProgram "inputs/day5"
  print $ solve1 =<< i
  print $ solve2 =<< i
