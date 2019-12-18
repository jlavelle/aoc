module Day16 where

import qualified Data.Text.IO as T
import qualified Data.Text as T
import Data.Char (digitToInt)
import Control.Lens (imap)
import Util (unpackWith)

phase :: [Int] -> [Int]
phase xs = imap go xs
  where
    go i _ = flip mod 10 . abs . sum $ zipWith (*) (pat i) xs
    pat i = drop 1 $ cycle $ case i of
      0 -> initial
      _ -> foldMap (replicate $ i + 1) initial
    initial = [0, 1, 0, -1]

fft :: Int -> [Int] -> [Int]
fft n xs = iterate phase xs !! n

solve1 :: [Int] -> [Int]
solve1 = take 8 . fft 100

solutions :: IO ()
solutions = do
  i <- unpackWith digitToInt . T.strip <$> T.readFile "inputs/day16"
  print $ solve1 i
