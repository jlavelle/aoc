module Day3 where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Read as T
import qualified Data.Text.IO as T
import Lens.Micro (over, _1, _2, (%~), (&), (<&>))
import Data.List (unfoldr)
import Data.Bitraversable (bisequence)

type WireNetwork = Map (Int, Int) Segment

data Segment = Segment
  { intersections :: Int
  , lengths :: [Int]
  }

instance Semigroup Segment where
  Segment i1 l1 <> Segment i2 l2 = Segment (i1 + i2) (l1 <> l2)

data Direction = R | U | L | D
  deriving Show

data Node = Node Direction Int
  deriving Show

parseNode :: Text -> Either String Node
parseNode t = T.splitAt 1 t & _1 %~ command & _2 %~ parseInt & bisequence <&> uncurry Node
  where
    parseInt = fmap fst . T.decimal
    command = \case
      "R" -> Right R
      "U" -> Right U
      "L" -> Right L
      "D" -> Right D
      _   -> Left "Unknown Direction"

toNetwork :: [Node] -> WireNetwork
toNetwork xs = foldr (\(p, l) -> Map.insert p (Segment 1 [l])) Map.empty $ unfoldr go ((0, 0), xs, 1)
  where
    go (_, [], _) = Nothing
    go (pos, node:ns, l) = case node of
      Node d n 
        | n <= 0    -> Just ((pos, l), (pos, ns, l))
        | otherwise -> 
            let pos' = delta d pos
            in Just ((pos', l), (pos', Node d (n - 1) : ns, l + 1))

    delta = \case
      R -> over _1 (+ 1)
      L -> over _1 (subtract 1)
      U -> over _2 (+ 1)
      D -> over _2 (subtract 1)

dist :: (Int, Int) -> (Int, Int) -> Int
dist (a, b) (c, d) = abs (c - a) + abs (d - b)

netIntersect :: [[Node]] -> WireNetwork
netIntersect = Map.filter ((>= 2) . intersections)
             . Map.unionsWith (<>)
             . fmap toNetwork

solve1 :: [[Node]] -> Int
solve1 = minimum
       . fmap (dist (0, 0))
       . Map.keys 
       . netIntersect

solve2 :: [[Node]] -> Int
solve2 = minimum
       . fmap (sum . lengths)
       . netIntersect

parseInput :: Text -> Either String [[Node]]
parseInput = traverse (traverse parseNode . T.splitOn ",") . T.lines

readInput :: IO (Either String [[Node]])
readInput = parseInput <$> T.readFile "inputs/day3"

solutions :: IO ()
solutions = do
  i <- readInput 
  print $ solve1 <$> i
  print $ solve2 <$> i

test1 :: Text
test1 = "R75,D30,R83,U83,L12,D49,R71,U7,L72\nU62,R66,U55,R34,D71,R55,D58,R83"
