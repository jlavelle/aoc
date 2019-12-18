module Day17 where

import IntCode (parseProgram, interpretOut)
import Linear.V2 (V2(..), _x, _y)
import qualified Data.Map as Map
import Data.Map (Map)
import Control.Lens (ifoldMap, (+~), (-~), (^.), (<&>))
import qualified Data.Text.IO as T
import qualified Data.Text as T
import Util (renderMap)
import Data.Witherable (mapMaybe)

data Direction = N | S | E | W
  deriving (Eq, Ord, Enum, Bounded, Show)

data Robot = OnPath Direction | Tumbling
  deriving (Eq, Show)

data Tile
  = Robot Robot
  | Scaffold
  deriving (Eq, Show)

renderTile :: Tile -> Char
renderTile = \case
  Robot r -> case r of
    OnPath d -> case d of
      N -> '^'
      E -> '>'
      S -> 'v'
      W -> '<'
    _ -> 'X'
  Scaffold -> '#'

parseTile :: Char -> Maybe Tile
parseTile = \case
  '^' -> Just $ Robot $ OnPath N
  'v' -> Just $ Robot $ OnPath S
  '>' -> Just $ Robot $ OnPath E
  '<' -> Just $ Robot $ OnPath W
  'X' -> Just $ Robot Tumbling
  '#' -> Just Scaffold
  '.' -> Nothing
  c   -> error $ "Unknown tile " <> show c

runASCII :: [Int] -> Map (V2 Int) Tile
runASCII = go . lines . fmap toEnum . flip interpretOut []
  where
    go = ifoldMap \y -> ifoldMap \x -> maybe mempty (Map.singleton (V2 x y)) . parseTile

intersections :: Map (V2 Int) Tile -> [V2 Int]
intersections m = filter go $ Map.keys m
  where
    go p = length (mapMaybe (`Map.lookup` m) $ neighbors p) == 4

neighbors :: V2 Int -> [V2 Int]
neighbors p = [_x +~ 1, _x -~ 1, _y +~ 1, _y -~ 1] <&> ($ p)

solve1 :: [Int] -> Int
solve1 = sum . fmap product . intersections . runASCII

solutions :: IO ()
solutions = do
  i <- parseProgram "inputs/day17"
  print $ solve1 i
  T.putStrLn $ renderMap (T.singleton . renderTile) "." (runASCII i)
