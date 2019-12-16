module Day10 where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Lens.Micro ((^?), ix)
import Data.Bifunctor (bimap)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Ord (comparing)
import Data.Foldable (maximumBy, fold)
import Data.List (sortBy, transpose)

type Point a = (a, a)

data Ray a = Ray a (Point a) 
  deriving (Eq, Ord, Show)

angle :: Ray a -> a
angle (Ray a _) = a

origin :: Ray a -> Point a
origin (Ray _ a) = a

convert :: (Integral a, Num b) => Point a -> Point b
convert = bimap fromIntegral fromIntegral

rayThrough :: (Eq a, RealFloat a) => Point a -> Point a -> Ray a
rayThrough p1@(x1, y1) (x2, y2) =
  let dx = x2 - x1
      dy = y2 - y1
  in Ray (atan2 (-dx) dy) p1

visibilities :: Set (Point Int) -> Map (Point Int) (Map (Ray Double) [Point Int])
visibilities field = foldr go mempty field
  where
    go a m = Map.insert a (foldr lns mempty $ Set.delete a field) m
      where
        lns b = Map.insertWith (<>) (rayThrough (convert a) (convert b)) [b]

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

stationLocation :: Set (Point Int) -> Map (Ray Double) [Point Int]
stationLocation = maximumBy (comparing length) . visibilities

solve1 :: Set (Point Int) -> Int
solve1 = length . stationLocation

solve2 :: Set (Point Int) -> Maybe (Point Int)
solve2 = (^? ix 199) . fold . transpose . fmap (sortBy (comparing pabs) . snd) . Map.toAscList . stationLocation
  where
    pabs (a, b) = (abs a, abs b)

solutions :: IO ()
solutions = do
  i <- parseInput
  print $ solve1 i
  print $ solve2 i

test1 :: Set (Point Int)
test1 = parseField ".#..#..###\n####.###.#\n....###.#.\n..###.##.#\n##.##.#.#.\n....###..#\n..#.#..#.#\n#..#.#.###\n.##...##.#\n.....#.#.."
