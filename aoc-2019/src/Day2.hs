module Day2 where

import Control.Lens ((^.), (^?), (&), (.~), ix, _1)
import IntCode2 (interpretSt, memory, parseProgram)

interpretNounVerb :: [Int] -> Int -> Int -> Maybe Int
interpretNounVerb xs n v = interpretSt is [] ^. _1 . memory ^? ix 0
  where
    is = xs & ix 1 .~ n
            & ix 2 .~ v

solve1 :: [Int] -> Maybe Int
solve1 xs = interpretNounVerb xs 12 2

solve2 :: [Int] -> Maybe Int
solve2 xs = foldr go Nothing [(x, y) | x <- [0..99], y <- [0..99]]
  where
    go (n, v) acc
      | ans == target = Just $ 100 * n + v
      | otherwise     = acc
      where
        ans = interpretNounVerb xs n v
    
    target = Just 19690720

solutions :: IO ()
solutions = do
  i <- parseProgram "inputs/day2"
  print $ solve1 i
  print $ solve2 i
