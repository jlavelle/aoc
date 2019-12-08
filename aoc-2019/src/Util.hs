module Util where

import Data.List (unfoldr)
import qualified Control.Foldl as F

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
