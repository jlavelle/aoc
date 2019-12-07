module Day2 where

import qualified Data.IntMap as IntMap
import Lens.Micro ((&), (.~), ix)
import qualified Data.Text as T
import qualified Data.Text.Read as T
import qualified Data.Text.IO as T
import IntCode (interpret, defaultHandle, ICState(..))

interpretNounVerb :: [Int] -> Int -> Int -> Maybe Int
interpretNounVerb xs n v = p0 $ interpret is defaultHandle
  where
    is = xs & ix 1 .~ n
            & ix 2 .~ v
    p0 = either (const Nothing) id . fmap (IntMap.lookup 0 . memory)

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
  i <- parseInput
  print $ solve1 <$> i
  print $ solve2 <$> i

parseInput :: IO (Either String [Int])
parseInput = traverse (fmap fst . T.decimal) . T.splitOn "," <$> T.readFile "inputs/day2"
