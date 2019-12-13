{-# LANGUAGE BangPatterns #-}

module Day12 where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Read as T
import qualified Data.Text.IO as T
import Data.Foldable (foldl')
import Data.Monoid (Sum(..))
import Linear.V3 (V3(..))
import Control.Applicative (liftA2)
import Data.Bool (bool)
import Data.Either (isLeft)

data Moon = Moon
  { moonPos :: V3 Int
  , moonVel :: V3 Int
  } deriving (Show, Eq, Ord)

step :: (Functor f, Foldable f) => f Moon -> f Moon
step s = fmap go s
  where
    go m = applyVelocity $ foldl' gravity m s

energy :: Foldable f => f Moon -> Sum Int
energy = foldMap go
  where
    go m = sabs (moonPos m) * sabs (moonVel m)
    sabs = foldMap (Sum . abs)

simulate :: (Foldable f, Functor f) => f Moon -> [f Moon]
simulate = iterate step

simulateN :: (Functor f, Foldable f) => Int -> f Moon -> f Moon
simulateN n s = simulate s !! n

applyVelocity :: Moon -> Moon
applyVelocity m = m { moonPos = moonPos m + moonVel m }

-- Applies the gravity effects from the 2nd arg to the 1st arg
gravity :: Moon -> Moon -> Moon
gravity m m' =
  let ds = liftA2 mkDelta (moonPos m) (moonPos m')
  in m { moonVel = ds <*> moonVel m }
  where
    mkDelta a b = case compare a b of
      LT -> (+1)
      GT -> subtract 1
      EQ -> id

parseMoon :: Text -> Either String Moon
parseMoon = toMoon . traverse inner . outer
  where
    inner = parseInt . snd . T.breakOnEnd "=" . T.strip
    outer = T.splitOn "," . T.init . T.tail
    parseInt = fmap fst . T.signed T.decimal
    toMoon = \case
      Right [x, y, z] -> Right $ Moon (V3 x y z) (V3 0 0 0)
      _ -> Left "No parse"

parseSystem :: Text -> Either String [Moon]
parseSystem = traverse parseMoon . T.lines

parseInput :: IO (Either String [Moon])
parseInput = parseSystem <$> T.readFile "inputs/day12"

solve1 :: [Moon] -> Sum Int
solve1 = energy . simulateN 1000

solve2 :: [Moon] -> Int
solve2 s = foldr1 lcm $ fmap (either id id) $ loop (V3 (Right 1) (Right 1) (Right 1)) s
  where
    initPos = traverse moonPos s
    loop n x =
      let x' = step x
          eq = liftA2 (==) (traverse moonPos x') initPos
          n' = bool id (either Left Left) <$> eq <*> fmap (fmap (+1)) n
      in bool (loop n' x') n' $ all isLeft n'

solutions :: IO ()
solutions = do
  Right i <- parseInput
  print $ solve1 i
  print $ solve2 i

test1 :: [Moon]
test1 =
  let Right m = parseSystem "<x=-8, y=-10, z=0>\n<x=5, y=5, z=10>\n<x=2, y=-7, z=3>\n<x=9, y=-8, z=-3>"
  in m
