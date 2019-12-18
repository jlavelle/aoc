module Day7 where

import IntCode (interpretOut, parseProgram', parseProgram)
import Data.Monoid (Endo(..))
import Data.List (permutations)
import Data.Profunctor (lmap)
import Data.Function (fix)

programs :: [Int] -> [Int] -> [[Int] -> [Int]]
programs ps prog = fmap (appEndo . foldMap go) $ permutations ps
  where
    go i = Endo $ lmap (i:) $ interpretOut prog

maxAmp :: [[Int] -> [Int]] -> Int
maxAmp = maximum . fmap (last . ($ [0]))

solve1 :: [Int] -> Int
solve1 = maxAmp . programs [0..4]

solve2 :: [Int] -> Int
solve2 = maxAmp . fmap loopPipe . programs [5..9]
  where
    loopPipe p i = fix \out -> p (i <> out)

solutions :: IO ()
solutions = do
  i <- parseProgram "inputs/day7"
  print $ solve1 i
  print $ solve2 i
