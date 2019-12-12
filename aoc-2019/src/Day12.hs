module Day12 where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Read as T
import qualified Data.Text.IO as T
import Lens.Micro (each, (^..), (&))
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Foldable (foldl')
import Data.List (unfoldr)
import Data.Monoid (Sum(..))

type Vec3 = (Int, Int, Int)

data Moon = Moon
  { moonPos :: Vec3
  , moonVel :: Vec3
  , moonId  :: Int
  } deriving (Show, Eq, Ord)

step :: Set Moon -> Set Moon
step s = Set.map go s
  where
    go m = applyVelocity $ foldl' gravity m $ Set.delete m s

energy :: Set Moon -> Sum Int
energy = foldMap go
  where
    go m = sumVec (moonPos m) * sumVec (moonVel m)
    sumVec v = v ^.. each & foldMap (Sum . abs)

simulate :: Set Moon -> [Set Moon]
simulate = unfoldr \x -> Just (x, step x)

simulateN :: Int -> Set Moon -> Set Moon
simulateN n s = simulate s !! n

applyVelocity :: Moon -> Moon
applyVelocity m =
  let [x, y, z] = zipWith (+) (moonPos m ^.. each) (moonVel m ^.. each)
  in m { moonPos = (x, y, z) }

-- Applies the gravity effects from the 2nd arg to the 1st arg
gravity :: Moon -> Moon -> Moon
gravity m m' =
  let ds = zipWith mkDelta (moonPos m ^.. each) (moonPos m' ^.. each)
      [vx,vy,vz] = zipWith ($) ds (moonVel m ^.. each)
  in m { moonVel = (vx, vy, vz) }
  where
    mkDelta a b = case compare a b of
      LT -> (+1)
      GT -> subtract 1
      EQ -> id

parseMoon :: Int -> Text -> Either String Moon
parseMoon i = toMoon . traverse inner . outer
  where
    inner = parseInt . snd . T.breakOnEnd "=" . T.strip
    outer = T.splitOn "," . T.init . T.tail
    parseInt = fmap fst . T.signed T.decimal
    toMoon = \case
      Right [x, y, z] -> Right $ Moon (x, y, z) (0, 0, 0) i
      _ -> Left "No parse"

parseSystem :: Text -> Either String (Set Moon)
parseSystem = fmap Set.fromList . traverse (uncurry parseMoon) . zip [0..] . T.lines

parseInput :: IO (Either String (Set Moon))
parseInput = parseSystem <$> T.readFile "inputs/day12"

solve1 :: Set Moon -> Sum Int
solve1 = energy . simulateN 1000

solutions :: IO ()
solutions = do
  Right i <- parseInput
  print $ solve1 i
