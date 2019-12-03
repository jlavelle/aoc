module Day2 where

import qualified Data.IntMap as IntMap
import Data.List (unfoldr)
import Data.Foldable (foldl')
import Lens.Micro ((&), (.~), ix)
import qualified Data.Text as T
import qualified Data.Text.Read as T
import qualified Data.Text.IO as T

data Op
  = Add Int Int Int
  | Mul Int Int Int
  deriving Show

toProgram :: [Int] -> [Op]
toProgram is = unfoldr go is
  where
    go []     = Nothing
    go (99:_) = Nothing
    go as = case take 4 as of
      (a:b:c:d:_) | a == 1 -> Just (Add b c d, drop 4 as)
                  | a == 2 -> Just (Mul b c d, drop 4 as)
                  | otherwise -> Nothing
      _ -> Nothing

interpret :: [Int] -> Maybe Int
interpret xs = 
  let mem  = IntMap.fromList $ zip [0..] xs
      prog = toProgram xs
      res  = foldl' go (Just mem) prog
  in res >>= IntMap.lookup 0
  where
    go acc = \case
      Add a b c -> update (+) a b c acc
      Mul a b c -> update (*) a b c acc

    update f a b c mmem = do
      mem <- mmem
      av  <- IntMap.lookup a mem
      bv  <- IntMap.lookup b mem
      pure $ IntMap.insert c (f av bv) mem

interpretNounVerb :: [Int] -> Int -> Int -> Maybe Int
interpretNounVerb xs n v = interpret is
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
  i <- parseInput
  print $ solve1 <$> i
  print $ solve2 <$> i

parseInput :: IO (Either String [Int])
parseInput = traverse (fmap fst . T.decimal) . T.splitOn "," <$> T.readFile "inputs/day2"