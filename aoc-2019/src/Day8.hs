module Day8 where

import Util (chunks, count)
import qualified Data.Text.IO as T
import qualified Data.Text as T
import Data.Char (digitToInt)
import Data.Ord (comparing)
import qualified Control.Foldl as F
import Lens.Micro (_1)
import Lens.Micro.Extras (view)
import Control.Applicative (ZipList(..))
import Data.Monoid (First(..), Ap(..))
import Data.Bool (bool)

solve1 :: Int -> Int -> [Int] -> Maybe Int
solve1 w h = fmap (\(_, a, b) -> a * b) . F.fold go . chunks (w * h)
  where
    go = F.premap (F.fold lf) $ F.minimumBy (comparing $ view _1)
    lf = (,,) <$> count (== 0) <*> count (== 1) <*> count (== 2)

solve2 :: Int -> Int -> [Int] -> [Int]
solve2 w h = F.fold (F.foldMap f g) . chunks (w * h)
  where
    f = Ap . ZipList . fmap (\n -> First $ bool (Just n) Nothing (n == 2))
    g = fmap (maybe 2 id . getFirst) . getZipList . getAp

render :: Int -> [Int] -> T.Text
render w = foldMap (\x -> foldMap (bool " " "x" . (== 1)) x <> "\n") . chunks w

solutions :: IO ()
solutions = do
  i <- parseInput
  print $ solve1 25 6 i
  T.putStrLn $ render 25 $ solve2 25 6 i

parseInput :: IO [Int]
parseInput = T.foldr ((:) . digitToInt) [] . T.strip <$> T.readFile "inputs/day8"
