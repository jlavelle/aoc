module Day6 where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Lens.Micro (over, _2)
import Algebra.Graph.AdjacencyMap (AdjacencyMap)
import qualified Algebra.Graph.AdjacencyMap as AM
import qualified Algebra.Graph.ToGraph as G
import Data.Tree (foldTree, Tree(..))
import Data.Maybe (catMaybes)
import Data.Foldable (fold)
import Criterion.Main
import Data.Semigroup (Sum)

solve1 :: AdjacencyMap Text -> Sum Int
solve1 = foldMap sumDepths . subForest . head . G.dfsForestFrom ["COM"]

solve2 :: AdjacencyMap Text -> Maybe Int
solve2 = fmap (subtract 3 . length) . pathTo "SAN" . head . G.dfsForestFrom ["YOU"]

pathTo :: Ord a => a -> Tree a -> Maybe [a]
pathTo to = foldTree go
  where
    go x xs | x == to   = Just [x]
            | otherwise = case catMaybes xs of
                [] -> Nothing
                zs -> Just $ x : fold zs

sumDepths :: Tree a -> Sum Int
sumDepths = fst . foldTree go
  where
    go _ [] = (1, 1)
    go _ xs = let (cs, s) = fold xs in (1 + s + cs, 1 + s)

parseAM :: Text -> AdjacencyMap Text
parseAM t = let gr = go t in AM.overlay gr $ AM.transpose gr
  where
    go = AM.stars
       . fmap (over _2 (pure . T.drop 1) . T.breakOn ")")
       . T.lines

readInput :: IO (AdjacencyMap Text)
readInput = parseAM <$> T.readFile "inputs/day6"

test1 :: Text
test1 = "COM)B\nB)C\nC)D\nD)E\nE)F\nB)G\nG)H\nD)I\nE)J\nJ)K\nK)L"

solutions :: IO ()
solutions = do
  i <- readInput
  print $ solve1 i
  print $ solve2 i

benchmark :: IO ()
benchmark = defaultMain
  [ env readInput $ \i -> bgroup "Day 6"
    [ bench "Part 1" $ whnf solve1 i
    , bench "Part 2" $ whnf solve2 i]
  ]
