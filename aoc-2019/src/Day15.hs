{-# LANGUAGE TemplateHaskell #-}

module Day15 where

import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad.State (MonadState, evalState)
import Linear.V2 (V2(..), _x, _y)
import Control.Lens ((^.), (.~), (%~), (&), (+~), (-~), use, (.=), (%=), (<&>))
import Control.Lens.TH (makeLenses)
import IntCode2 (compile, initICState, IntCode(..), parseProgram)
import Data.Witherable (wither)
import Data.Foldable (fold)

data Direction = N | S | W | E
  deriving (Show, Enum, Bounded)

data Response
  = Wall
  | Step
  | Goal
  deriving (Show, Enum)

data Node
  = WallNode
  | Unvisited
  | Visted

data Robot = Robot
  { _brain    :: IntCode Int
  , _path     :: [Direction]
  , _position :: V2 Int
  }

makeLenses ''Robot

data Explore = Explore
  { _robots  :: [Robot]
  , _visited :: Set (V2 Int)
  }

initialState :: [Int] -> Explore
initialState p = Explore [Robot ic [] (V2 0 0)] mempty
  where
    ic = compile $ initICState p

makeLenses ''Explore

printDirection :: Direction -> Int
printDirection = succ . fromEnum

parseResponse :: Int -> Response
parseResponse = toEnum

explore :: MonadState Explore m => m [Direction]
explore = do
  ers <- traverse roboBfs =<< use robots
  case fmap fold $ sequenceA ers of
    Left p   -> pure p
    Right rs -> (robots .= rs) *> explore

roboBfs :: MonadState Explore m => Robot -> m (Either [Direction] [Robot])
roboBfs r = do
  (ds, vs) <- use visited <&> unzip . candidates
  visited %= Set.union (Set.fromList vs)
  pure $ wither (stepRobot r) ds
  where
    candidates v = filter (not . flip Set.member v . snd) (adjacent $ r ^. position)

stepRobot :: Robot -> Direction -> Either [Direction] (Maybe Robot)
stepRobot r d = case (r ^. brain) of
  Input f -> case f $ printDirection d of
    Output a next -> case parseResponse a of
      Wall -> Right Nothing
      Step -> Right $ Just $ r & position %~ move d & brain .~ next & path %~ (d:)
      Goal -> Left $ d : r ^. path
    _ -> die
  _ -> die
  where
    die = error "Unexpected robot state"

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
solve1 = length . evalState explore . initialState

solutions :: IO ()
solutions = do
  i <- parseProgram "inputs/day15"
  print $ solve1 i