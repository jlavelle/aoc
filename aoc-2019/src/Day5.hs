module Day5 where

import IntCode2 (interpretP, ICState(..), Program, Error, parseProgram)
import Data.Sequence (Seq)

diagnostic :: [Int] -> Program Int -> Either (Error Int) (Seq Int)
diagnostic i p = fmap fst $ interpretP p i

solve1 :: Program Int -> Either (Error Int) (Seq Int)
solve1 = diagnostic [1]

solve2 :: Program Int -> Either (Error Int) (Seq Int)
solve2 = diagnostic [5]

solutions :: IO ()
solutions = do
  Right i <- parseProgram "inputs/day5"
  print $ solve1 i
  print $ solve2 i
