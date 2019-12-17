module Util where

import Data.List (unfoldr)
import qualified Control.Foldl as F
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Text as T
import Data.Text (Text)
import Linear.V2 (V2(..), _x, _y)
import Data.Ord (comparing)
import Control.Lens ((^.), (&))
import Data.Maybe (fromJust)

digits :: Integral a => a -> [a]
digits = loop []
  where
    loop acc 0 = acc
    loop acc n =
      let (q, r) = quotRem n 10
      in loop (r : acc) q

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
        comp f l = fmap ((^. l) . fromJust) $ f $ comparing (^. l)
