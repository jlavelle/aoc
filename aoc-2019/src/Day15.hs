{-# LANGUAGE TemplateHaskell #-}

module Day15 where

import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Control.Monad.State (MonadState, evalState, execState)
import Linear.V2 (V2(..), _x, _y)
import Control.Lens ((^.), (.~), (%~), (&), (+~), (-~), use, (.=), (%=), (<&>), (<>=))
import Control.Lens.TH (makeLenses)
import IntCode (compile, initICState, IntCode(..), parseProgram)
import Data.Foldable (fold)
import Algebra.Graph.AdjacencyMap (AdjacencyMap)
import qualified Algebra.Graph.AdjacencyMap as AM
import Data.Monoid (First(..))
import Data.Functor (($>))
import Control.Monad (unless)
import Util (bfsFrom)
import Data.Text (Text)
import Data.Tuple (swap)

data Direction = N | S | W | E
  deriving (Show, Enum, Bounded)

data Tile
  = Wall
  | Empty
  | Goal
  | Start
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
    start = Node (V2 0 0) Start
    ic = compile $ initICState p

printDirection :: Direction -> Int
printDirection = succ . fromEnum

parseResponse :: Int -> Tile
parseResponse = \case
  0 -> Wall
  1 -> Empty
  2 -> Goal
  _ -> error "Invalid response"

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

roboFlood :: MonadState Explore m => Robot -> m [Robot]
roboFlood r = do
  (ds, vs) <- use visited <&> unzip . candidates
  visited %= Set.union (Set.fromList vs)
  traverse (stepRobot r) ds
  where
    candidates v = filter (not . flip Set.member v . snd) (adjacent $ r ^. node . position)

stepRobot :: MonadState Explore m => Robot -> Direction -> m Robot
stepRobot r d = case (r ^. brain) of
  Input f -> case f $ printDirection d of
    Output a next -> update next $ parseResponse a
    _ -> die
  _ -> die
  where
    die = error "Unexpected robot state"
    update next t = insertNode *> robot
      where
        robot = case t of
          Wall -> pure $ r & brain .~ next
          Goal -> (goalPath <>= First (Just (d : r ^. path))) $> advance
          _    -> pure advance
        advance = r & node  .~ node'
                    & brain .~ next 
                    & path  %~ (d:)
        insertNode = roomMap %= AM.overlay es
        node' = r ^. node & position %~ move d
                          & tile     .~ t
        es | t == Wall = AM.vertex node'
           | otherwise = let e = (r ^. node, node') in AM.edges [e, swap e]

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

solve2 :: [Int] -> Int
solve2 p =
  let rm  = execState explore (initialState p) ^. roomMap
      [g] = goal rm
      bfs = bfsFrom g rm
  in maximum (fmap length bfs) - 1
  where
    goal = filter (\x -> x ^. tile == Goal) . Map.keys . AM.adjacencyMap

solutions :: IO ()
solutions = do
  i <- parseProgram "inputs/day15"
  print $ solve1 i
  print $ solve2 i

renderNode :: Node -> Text
renderNode (Node _ t) = case t of
  Wall  -> "X"
  Empty -> " "
  Goal  -> "O"
  Start -> "S"
