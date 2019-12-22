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
import Control.Lens ((^.), (&), use, (.=), (%=), ifoldMap, Getter, ifoldr, (+~), (-~), (<&>), FoldableWithIndex, _1, _2, _3)
import Control.Lens.TH (makeLenses)
import Data.Maybe (fromJust)
import Algebra.Graph.AdjacencyMap (AdjacencyMap)
import qualified Algebra.Graph.AdjacencyMap as AM
import Algebra.Graph.Class (Graph)
import qualified Algebra.Graph.Class as G
import Control.Monad.State (State, execState, evalState, get, modify)
import Control.Monad (unless, when)
import Data.Foldable (fold, for_)
import Data.List.NonEmpty (NonEmpty(..))
import Control.Comonad (extend)
import qualified Data.List.NonEmpty as NE
import Data.Monoid (Sum(..))
import Data.Witherable (mapMaybe)
import Data.Tree (Tree, unfoldTreeM_BF)

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
    yr = [miny..maxy]
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

-- TODO Consolidate with bfsFromTo
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

bfsTreeFrom :: forall a. Ord a => a -> AdjacencyMap a -> Tree a
bfsTreeFrom s g = evalState (unfoldTreeM_BF go s) Set.empty
  where
    go n = do
      vs <- get
      let adj = Set.difference (adjacent n g) vs
      modify $ Set.union adj
      pure (n, Set.toList adj)

bfsFromTo :: forall a. Ord a => a -> Set a -> AdjacencyMap a -> Map a (NonEmpty a)
bfsFromTo s t g = execState loop (Set.singleton s, [pure s], Map.empty) ^. _3
  where
    loop = do
      qs <- use _2
      unless (null qs) do
        qs' <- traverse search qs
        _2 .= fold qs'
        loop

    search as = do
      vs <- use _1
      let adj = Set.difference (adjacent (NE.head as) g) vs
      for_ adj \a -> when (a `Set.member` t) $ _3 %= Map.insert a (NE.cons a as)
      _1 %= Set.union adj
      pure $ fmap (`NE.cons` as) $ Set.toList adj

adjacent :: Ord a => a -> AdjacencyMap a -> Set a
adjacent a = fold . Map.lookup a . AM.adjacencyMap

unpackWith :: (Char -> a) -> Text -> [a]
unpackWith f = T.foldr ((:) . f) []

eqOn :: Eq b => Getter a b -> a -> a -> Bool
eqOn l x y = x ^. l == y ^. l

intersperseMap :: (a -> a -> b) -> (a -> b) -> [a] -> [b]
intersperseMap _ _ [] = []
intersperseMap f g as = fold $ NE.toList $ extend go $ NE.fromList as
  where
    go (a :| [])    = [g a]
    go (a :| (b:_)) = [g a, f a b]

data Node a = Node
  { _position :: V2 Int
  , _tile     :: a
  } deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

makeLenses ''Node

mkGraph2D :: (Graph g, G.Vertex g ~ Node a) => Map (V2 Int) a -> g
mkGraph2D m = ifoldr go G.empty m
  where
    go p x g =
      let ns = mapMaybe (\y -> Node y <$> Map.lookup y m) (neighbors2D p)
      in G.overlay g (G.edges $ (Node p x,) <$> ns)

neighbors2D :: V2 Int -> [V2 Int]
neighbors2D p = [_x +~ 1, _x -~ 1, _y +~ 1, _y -~ 1] <&> ($ p)

parseGrid :: (Ord i, FoldableWithIndex i f) => (a -> Maybe b) -> f (f a) -> Map (V2 i) b
parseGrid f = ifoldMap \y -> ifoldMap \x -> maybe mempty (Map.singleton (V2 x y)) . f
