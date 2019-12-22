{-# LANGUAGE TemplateHaskell #-}

module Day18 where

import Linear.V2 (V2(..), _x, _y)
import Util (mkGraph2D, parseGrid, Node(..), position, tile, bfsFromTo)
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Algebra.Graph.AdjacencyMap as AM
import qualified Algebra.Graph.AdjacencyMap.Algorithm as AM
import Algebra.Graph.AdjacencyMap (AdjacencyMap)
import Data.Char (isUpper, isLower, toLower)
import Control.Lens (ifind, (^.), ix, (.~), (&))
import Control.Lens.TH (makePrisms)
import Control.Lens.Extras (is)
import Data.Maybe (fromJust)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Ord (comparing)
import Data.Foldable (minimumBy)

data Tile
  = Wall
  | Empty
  | Door Char
  | Key Char
  | You
  deriving (Eq, Ord, Show)

makePrisms ''Tile

parseTile :: Char -> Tile
parseTile = \case
  '#' -> Wall
  '.' -> Empty
  '@' -> You
  c | isUpper c -> Door c
    | isLower c -> Key c
    | otherwise -> error $ "Unknown char " <> [c]

mkGraph :: Map (V2 Int) Tile -> AdjacencyMap (Node Tile)
mkGraph m =
  let (w, nw) = Map.partition walkable m
  in AM.overlay (mkGraph2D w) (mkGraph2D nw)

walkable :: Tile -> Bool
walkable = \case
  Wall   -> False
  Door _ -> False
  _      -> True

findYou :: Map (V2 Int) Tile -> Node Tile
findYou = uncurry Node . fromJust . ifind (const $ is _You)

openDoor :: Char -> Map (V2 Int) Tile -> Map (V2 Int) Tile
openDoor c m =
  let Just (p, _) = ifind door m
  in m & ix p .~ Empty
  where
    door _ (Door c') = toLower c == toLower c'
    door _ _ = False

parseInput :: IO (Map (V2 Int) Tile)
parseInput = parseGrid (Just . parseTile) . lines <$> readFile "inputs/day18"

-- Upper bound path that always takes the next shortest route
greedyPath :: Map (V2 Int) Tile -> [(Node Tile, Int)]
greedyPath m =
  let g  = mkGraph m
      y  = findYou m
      ks = Set.fromList $ filter (\k -> k ^. tile & is _Key) $ AM.reachable y g
      p@(n@(Node kp (Key x)) :| _) = minimumBy (comparing length) $ bfsFromTo y ks g
      m' = m & openDoor x & ix kp .~ You & ix (y ^. position) .~ Empty
  in if null ks then [] else (n, length p) : greedyPath m'

solutions :: IO ()
solutions = do
  m <- parseInput
  traverse print $ greedyPath m
  pure ()
