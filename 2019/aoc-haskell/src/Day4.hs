module Day4 where

import Data.List (groupBy)
import Data.Monoid (All(..))

digits :: Integral a => a -> [a]
digits = loop []
  where
    loop acc 0 = acc
    loop acc n =
      let (q, r) = quotRem n 10
      in loop (r : acc) q

validPasswords :: [[Int] -> Bool] -> (Int, Int) -> [Int]
validPasswords rules (l, u) = [ x | x <- [l..u], valid x ]
  where
    valid n = let ds = digits n in getAll $ foldMap (All . ($ ds)) rules

twoAdjacent :: [Int] -> Bool
twoAdjacent = any (>= 2) . fmap length . groupBy (==)

onlyTwoAdjacent :: [Int] -> Bool
onlyTwoAdjacent = any (== 2) . fmap length . groupBy (==)

nonDecreasing :: [Int] -> Bool
nonDecreasing ds = all (uncurry (<=)) $ zip ds (drop 1 ds)

input :: (Int, Int)
input = (246540, 787419)

solve1 :: Int
solve1 = length $ validPasswords [twoAdjacent, nonDecreasing] input

solve2 :: Int
solve2 = length $ validPasswords [onlyTwoAdjacent, nonDecreasing] input

solutions :: IO ()
solutions = do
  print solve1
  print solve2
