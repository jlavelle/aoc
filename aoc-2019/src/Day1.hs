module Day1 where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Read as T
import Data.Monoid (Sum(..))
import Data.Foldable (traverse_)
import Data.List (unfoldr)

fuelReq :: Integral a => a -> a
fuelReq n = div n 3 - 2

fuelFuelReq :: Integral a => a -> a
fuelFuelReq = foldr (+) 0 . unfoldr go . fuelReq
  where
    go x | x <= 0    = Nothing
         | otherwise = Just (x, fuelReq x)

solve1 :: (Foldable f, Integral a) => f a -> Sum a
solve1 = foldMap (Sum . fuelReq)

solve2 :: (Foldable f, Integral a) => f a -> Sum a
solve2 = foldMap \a -> 
  let mreq = fuelReq a
  in Sum mreq <> Sum (fuelFuelReq mreq)

solutions :: IO ()
solutions = do
  ei <- parseInput
  traverse_ (print . solve1) ei
  traverse_ (print . solve2) ei

-- TODO Utility module
parseInput :: IO (Either String [Int])
parseInput = traverse (fmap fst . T.decimal) . T.lines <$> T.readFile "inputs/day1"