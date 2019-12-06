module Day6 where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Lens.Micro (over, _2)
import Algebra.Graph.AdjacencyMap (AdjacencyMap)
import qualified Algebra.Graph.AdjacencyMap as AM
import qualified Algebra.Graph.ToGraph as G
import Data.Tree (foldTree)
import Data.Maybe (catMaybes)
import Data.Foldable (fold, minimumBy)
import Data.Ord (comparing)
import Criterion.Main

solve1 :: AdjacencyMap Text -> Int
solve1 = fst . foldTree go . head . G.dfsForestFrom ["COM"]
  where
    go _ [] = (1, 1)
    go l xs =
      let (cs, s) = unzip xs
          s' = if l == "COM" then 0 else sum s + 1
      in (s' + sum cs, sum s + 1)

solve2 :: AdjacencyMap Text -> Maybe Int
solve2 = fmap (subtract 3 . length) . pathFromTo "YOU" "SAN"

pathFromTo :: Ord a => a -> a -> AdjacencyMap a -> Maybe [a]
pathFromTo from to = foldTree go . head . G.dfsForestFrom [from]
  where
    go x xs | x == to   = Just [x]
            | otherwise = case catMaybes xs of
                [] -> Nothing
                zs -> Just $ x : minimumBy (comparing length) zs

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
