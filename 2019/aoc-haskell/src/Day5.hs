module Day5 where

import IntCode (interpret, ICState(..), Handle(..), defaultHandle)
import qualified Data.Text as T
import qualified Data.Text.Read as T
import qualified Data.Text.IO as T

solve1 :: [Int] -> Either String [Int]
solve1 xs = fmap (output . handle) $ interpret xs $ defaultHandle { input = [1] }

solutions :: IO ()
solutions = do
  i <- parseInput
  print $ solve1 =<< i

parseInput :: IO (Either String [Int])
parseInput = traverse (fmap fst . T.signed T.decimal) . T.splitOn "," <$> T.readFile "inputs/day5"
