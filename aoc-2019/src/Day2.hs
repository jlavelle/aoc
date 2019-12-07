module Day2 where

import qualified Data.IntMap as IntMap
import Lens.Micro ((&), (.~), ix)
import IntCode (interpret, defaultHandle, ICState(..), Program(..), parseProgram)

interpretNounVerb :: Program -> Int -> Int -> Maybe Int
interpretNounVerb (Program xs) n v = p0 $ interpret (Program is) defaultHandle
  where
    is = xs & ix 1 .~ n
            & ix 2 .~ v
    p0 = either (const Nothing) id . fmap (IntMap.lookup 0 . memory)

solve1 :: Program -> Maybe Int
solve1 xs = interpretNounVerb xs 12 2

solve2 :: Program -> Maybe Int
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
