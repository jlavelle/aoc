{-# LANGUAGE TemplateHaskell #-}

module Day15 where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Control.Monad.State (MonadState, evalState, execState)
import Linear.V2 (V2(..), _x, _y)
import Control.Lens ((^.), (.~), (%~), (&), (+~), (-~), use, (.=), (%=), (<&>), (<>=), zoom, _1, _2, (+=), each, (^..))
import Control.Lens.TH (makeLenses)
import IntCode2 (compile, initICState, IntCode(..), parseProgram)
import Data.Witherable (wither)
import Data.Foldable (fold, traverse_)
import Algebra.Graph.AdjacencyMap (AdjacencyMap)
import qualified Algebra.Graph.AdjacencyMap as AM
import Data.Monoid (First(..))
import Data.Functor (($>))
import Control.Monad (unless)
import Data.Maybe (fromJust)

data Direction = N | S | W | E
  deriving (Show, Enum, Bounded)

data Tile
  = Wall
  | Empty
  | Goal
  deriving (Show, Eq, Ord, Enum)

data Node = Node 
  { _position :: V2 Int
  , _tile     :: Tile
  } deriving (Show, Eq, Ord)

makeLenses ''Node

data Robot = Robot
  { _brain :: IntCode Int
  , _path  :: [Direction]
  , _node  :: Node
  }

makeLenses ''Robot

data Explore = Explore
  { _robots   :: [Robot]
  , _visited  :: Set (V2 Int)
  , _roomMap  :: AdjacencyMap Node
  , _goalPath :: First [Direction]
  }

makeLenses ''Explore

initialState :: [Int] -> Explore
initialState p = Explore [initRobot] (Set.singleton $ V2 0 0) (AM.vertex start) (First Nothing)
  where
    initRobot = Robot ic [] start
    start = Node (V2 0 0) Empty
    ic = compile $ initICState p

printDirection :: Direction -> Int
printDirection = succ . fromEnum

parseResponse :: Int -> Tile
parseResponse = toEnum

exploreStep :: MonadState Explore m => m ()
exploreStep = do
  rs <- traverse roboFlood =<< use robots
  robots .= fold rs

findGoal :: MonadState Explore m => m [Direction]
findGoal = do
  First mg <- use goalPath
  case mg of
    Just d  -> pure d
    Nothing -> exploreStep *> findGoal

explore :: MonadState Explore m => m ()
explore = use robots >>= \rs -> unless (null rs) (exploreStep *> explore)

-- -- TODO Consolidate with roboFill
-- floodFill :: MonadState Explore m => m ()
-- floodFill = do
--   explore
--   gl <- use roomMap <&> getGoal . AM.adjacencyMap
--   visited .= Set.singleton (gl ^. position)
--   robots  .= [Robot Halted [] gl]
--   loop
--   where
--     loop = do
--       rm <- use roomMap
--       vs <- use visited

--     -- loop ns = do
--     --   _1 += 1
--     --   adj <- use (_2 . roomMap) <&> \am -> foldMap (\n -> Set.toList $ fold $ Map.lookup n (AM.adjacencyMap am)) ns
--     --   vs  <- use (_2 . visited) <&> \v -> filter (not . flip Set.member v . (^. position)) adj
--     --   _2 . visited %= Set.union (Set.fromList $ vs ^.. each . position)
--     --   loop vs

--     getGoal = head . filter (\x -> x ^. tile == Goal) . Map.keys

roboFlood :: MonadState Explore m => Robot -> m [Robot]
roboFlood r = do
  (ds, vs) <- use visited <&> unzip . candidates
  visited %= Set.union (Set.fromList vs)
  wither (stepRobot r) ds
  where
    candidates v = filter (not . flip Set.member v . snd) (adjacent $ r ^. node . position)

stepRobot :: MonadState Explore m => Robot -> Direction -> m (Maybe Robot)
stepRobot r d = case (r ^. brain) of
  Input f -> case f $ printDirection d of
    Output a next -> update next $ parseResponse a
    _ -> die
  _ -> die
  where
    die = error "Unexpected robot state"
    update next t = unless (t == Wall) insertNode *> robot
      where
        robot = case t of
          Wall -> pure Nothing
          Goal -> (goalPath <>= First (Just (d : r ^. path))) $> Just advance
          _    -> pure $ Just advance
        advance = r & node . position %~ move d 
                    & brain .~ next 
                    & path %~ (d:)
        insertNode = roomMap %= AM.overlay (AM.edge mkNode (r ^. node))
          where
            mkNode = r ^. node & position %~ move d 
                               & tile     .~ t

adjacent :: V2 Int -> [(Direction, V2 Int)]
adjacent p = zipWith (\a b -> (a, move a b)) ds $ replicate (length ds) p
  where
    ds = enumFromTo minBound maxBound

move :: Direction -> V2 Int -> V2 Int
move = \case
  N -> _y +~ 1
  S -> _y -~ 1
  E -> _x +~ 1
  W -> _x -~ 1

solve1 :: [Int] -> Int
solve1 = length . evalState findGoal . initialState

-- solve2 :: [Int] -> Int
-- solve2 = maximum . go . execState floodFill . initialState
--   where
--     go x = x ^. robots <&> \r -> length (r ^. path)

solutions :: IO ()
solutions = do
  i <- parseProgram "inputs/day15"
  print $ solve1 i
  print $ solve2 i