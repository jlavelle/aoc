module Day10 where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Functor ((<&>))
import Data.Bifunctor (bimap)

type Point a = (a, a)

data Line a
  = Line a a
  | Vertical a
  deriving (Eq, Ord, Show)

type AsteroidField = Set (Point Int)
type VisMap = Map (Point Int) Int

convert :: (Integral a, Num b) => Point a -> Point b
convert = bimap fromIntegral fromIntegral

lineThrough :: (Eq a, Fractional a) => Point a -> Point a -> Line a
lineThrough (x1, y1) (x2, y2) = maybe (Vertical x1) id $ Line <$> m <*> b
  where
    m = case x2 - x1 of
      0  -> Nothing
      dx -> Just $ (y2 - y1) / dx
    b = m <&> \m' -> y1 - m' * x1

visibilities :: AsteroidField -> VisMap
visibilities field = foldr go mempty field
  where
    go x m = undefined
