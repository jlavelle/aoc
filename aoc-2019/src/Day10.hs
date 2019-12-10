module Day10 where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Functor ((<&>))
import Data.Bifunctor (bimap)
import qualified Data.Text as T
import qualified Data.Text.IO as T

type Point a = (a, a)

data Ray a = Ray a (Point a) deriving (Eq, Ord, Show)

convert :: (Integral a, Num b) => Point a -> Point b
convert = bimap fromIntegral fromIntegral

rayThrough :: (Eq a, RealFloat a) => Point a -> Point a -> Ray a
rayThrough p1@(x1, y1) (x2, y2) =
  let dx = x2 - x1
      dy = y2 - y1
  in Ray (atan2 dy dx) p1

visibilities :: Set (Point Int) -> Map (Point Int) Int
visibilities field = foldr go mempty field
  where
    go a m =
      let ls = foldr lines (mempty :: Set (Ray Double)) field
      in Map.insert a (length ls) m
      where
        lines b = Set.insert (rayThrough (convert a) (convert b))

toField :: [[Bool]] -> Set (Point Int)
toField = foldMap go . zip [0..]
  where
    go (y, l) = Set.fromList 
              $ fmap fst 
              $ filter snd 
              $ zipWith (\x b -> ((x, y), b)) [0..] l

parseField :: T.Text -> Set (Point Int)
parseField = toField . fmap (T.foldr (\c a -> (c == '#') : a) []) . T.lines

parseInput :: IO (Set (Point Int))
parseInput = parseField <$> T.readFile "inputs/day10"

solutions :: IO ()
solutions = do
  i <- parseInput
  print $ maximum $ visibilities i

test1 :: Set (Point Int)
test1 = parseField ".#..#..###\n####.###.#\n....###.#.\n..###.##.#\n##.##.#.#.\n....###..#\n..#.#..#.#\n#..#.#.###\n.##...##.#\n.....#.#.."