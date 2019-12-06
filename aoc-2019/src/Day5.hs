module Day5 where

import IntCode (interpret, ICState(..), Handle(..), defaultHandle)
import qualified Data.Text as T
import qualified Data.Text.Read as T
import qualified Data.Text.IO as T

diagnostic :: [Int] -> [Int] -> Either String [Int]
diagnostic i p = fmap (output . handle) $ interpret p $ defaultHandle { input = i }

solve1 :: [Int] -> Either String [Int]
solve1 = diagnostic [1]

solve2 :: [Int] -> Either String [Int]
solve2 = diagnostic [5]

solutions :: IO ()
solutions = do
  i <- parseInput
  print $ solve1 =<< i
  print $ solve2 =<< i

parseInput :: IO (Either String [Int])
parseInput = traverse (fmap fst . T.signed T.decimal) . T.splitOn "," <$> T.readFile "inputs/day5"
