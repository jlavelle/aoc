module Day5 where

import IntCode (interpret, ICState(..), Handle(..), Program, defaultHandle, parseProgram)

diagnostic :: [Int] -> Program -> Either String [Int]
diagnostic i p = fmap (output . handle) $ interpret p $ defaultHandle { input = i }

solve1 :: Program -> Either String [Int]
solve1 = diagnostic [1]

solve2 :: Program -> Either String [Int]
solve2 = diagnostic [5]

solutions :: IO ()
solutions = do
  i <- parseProgram "inputs/day5"
  print $ solve1 =<< i
  print $ solve2 =<< i
