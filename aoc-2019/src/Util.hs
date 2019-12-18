{-# LANGUAGE TemplateHaskell #-}

module Util where

import Data.List (unfoldr)
import qualified Control.Foldl as F
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Linear.V2 (V2(..), _x, _y)
import Data.Ord (comparing)
import Control.Lens ((^.), (&), use, (.=), (%=), ifoldMap)
import Control.Lens.TH (makeLenses)
import Data.Maybe (fromJust)
import Algebra.Graph.AdjacencyMap (AdjacencyMap)
import qualified Algebra.Graph.AdjacencyMap as AM
import Control.Monad.State (State, execState)
import Control.Monad (unless, when)
import Data.Foldable (fold)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Monoid (Sum(..))

digits :: Integral a => a -> [a]
digits = loop []
  where
    loop acc 0 = acc
    loop acc n =
      let (q, r) = quotRem n 10
      in loop (r : acc) q

fromDigits :: Integral a => [a] -> a
fromDigits = getSum . ifoldMap (\i x -> Sum $ x * 10 ^ i) . reverse

chunks :: Int -> [a] -> [[a]]
chunks n = unfoldr go
  where
    go [] = Nothing
    go xs = Just $ splitAt n xs

count :: (a -> Bool) -> F.Fold a Int
count p = F.prefilter p $ F.length

unfoldrM :: Monad m => (b -> m (Maybe (a, b))) -> b -> m [a]
unfoldrM f = go
  where
    go b = do
      m <- f b
      case m of
        Nothing      -> pure []
        Just (a, b') -> (a :) <$> go b'

enumIndex :: (Bounded a, Enum a) => Int -> a
enumIndex n = let as = enumFromTo minBound maxBound in as !! mod n (length as)

renderMap :: (a -> Text) -> Text -> Map (V2 Int) a -> Text
renderMap f e m = foldMap (\ps -> foldMap go ps <> "\n") [[V2 x y | x <- xr] | y <- yr]
  where
    go p = Map.findWithDefault e p $ fmap f m
    xr = [minx..maxx]
    yr = reverse [miny..maxy]
    (minx, maxx, miny, maxy) = Map.keys m & F.fold mms
      where
        mms = (,,,) <$> minV _x <*> maxV _x <*> minV _y <*> maxV _y
        minV = comp F.minimumBy
        maxV = comp F.maximumBy
        comp g l = fmap ((^. l) . fromJust) $ g $ comparing (^. l)

data BfsState a = BfsState
  { _visited :: Set a
  , _queued  :: [NonEmpty a]
  , _paths   :: [NonEmpty a]
  }

makeLenses ''BfsState

bfsFrom :: forall a. Ord a => a -> AdjacencyMap a -> [NonEmpty a]
bfsFrom a g = execState loop (BfsState (Set.singleton a) [pure a] []) ^. paths
  where
    loop = do
      qs <- use queued
      unless (null qs) do
        qs' <- traverse search qs
        queued .= fold qs'
        loop

    search :: NonEmpty a -> State (BfsState a) [NonEmpty a]
    search as = do
      vs <- use visited
      let adj = Set.difference (adjacent (NE.head as) g) vs
      when (null adj) $ paths %= (as:)
      visited %= Set.union adj
      pure $ fmap (flip NE.cons as) (Set.toList adj)

adjacent :: Ord a => a -> AdjacencyMap a -> Set a
adjacent a = fold . Map.lookup a . AM.adjacencyMap

unpackWith :: (Char -> a) -> Text -> [a]
unpackWith f = T.foldr ((:) . f) []
