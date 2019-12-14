module Day2 where

import qualified Data.Map as Map
import Lens.Micro ((&), (.~), ix)
import IntCode2 (interpretP, ICState(..), Program(..), parseProgram)

interpretNounVerb :: Program Int -> Int -> Int -> Maybe Int
interpretNounVerb (Program xs) n v = p0 $ interpretP (Program is) []
  where
    is = xs & ix 1 .~ n
            & ix 2 .~ v
    p0 = either (const Nothing) id . fmap (Map.lookup 0 . _memory . snd)

solve1 :: Program Int -> Maybe Int
solve1 xs = interpretNounVerb xs 12 2

solve2 :: Program Int -> Maybe Int
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
  print $ solve1 <$> i
  print $ solve2 <$> i
