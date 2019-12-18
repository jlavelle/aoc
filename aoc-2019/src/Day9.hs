module Day9 where

import IntCode (parseProgram, interpretOut)

solve :: Int -> [Int] -> Either String Int
solve i p = case interpretOut p [i] of
  [a] -> Right a
  as  -> Left $ "Invalid output: " <> show as

solve1 :: [Int] -> Either String Int
solve1 = solve 1

solve2 :: [Int] -> Either String Int
solve2 = solve 2

solutions :: IO ()
solutions = do
  i <- parseProgram "inputs/day9"
  print $ solve1 i
  print $ solve2 i
