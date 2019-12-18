{-# LANGUAGE TemplateHaskell #-}

module Day13 where

import Data.Map (Map)
import qualified Data.Map as Map
import Control.Lens ((^.), (&), (.~), (%~), ix)
import Control.Lens.TH (makeLenses)
import IntCode (interpretOut, parseProgram, IntCode(..), compile, initICState)
import Util (chunks)
import Linear.V2 (V2(..), _x)

data Tile
  = Empty
  | Wall
  | Block
  | Paddle
  | Ball
  deriving (Show, Enum, Eq, Ord)

data Cell
  = TileCell Tile (V2 Int)
  | Score Int
  deriving (Show, Eq, Ord)

data GameState = GameState
  { _board   :: Map (V2 Int) Tile
  , _ball    :: V2 Int
  , _paddle  :: V2 Int
  , _score   :: Int
  } deriving Show

makeLenses ''GameState

initGame :: GameState
initGame = GameState mempty (V2 0 0) (V2 0 0) 0

update :: Cell -> GameState -> GameState
update (Score n) = score .~ n
update (TileCell t p) = f . g
  where
    f = board %~ Map.insert p t
    g = case t of
      Ball   -> ball   .~ p
      Paddle -> paddle .~ p
      _      -> id

autoPlay :: [Int] -> GameState
autoPlay = go initGame . compile . initICState
  where
    go g = \case
      Error e -> error $ show e
      Halted  -> g
      Input f -> case compare (g ^. paddle ^. _x) (g ^. ball ^. _x) of
        GT -> go g $ f (-1)
        LT -> go g $ f 1
        EQ -> go g $ f 0
      Output a (Output b (Output c next)) ->
        let g' = update (parseCell' a b c) g
        in go g' next
      _ -> error "Unexpected output"

cellTile :: Cell -> Maybe Tile
cellTile (TileCell t _) = Just t
cellTile _ = Nothing

parseCell :: [Int] -> Either String Cell
parseCell [a, b, c] = case (a, b) of
  (-1, 0) -> Right $ Score c
  _       -> Right $ TileCell (toEnum c) (V2 a b)
parseCell _ = Left "Unexpected number of arguments"

parseCell' :: Int -> Int -> Int -> Cell
parseCell' a b c = case (a, b) of
  (-1, 0) -> Score c
  _       -> TileCell (toEnum c) (V2 a b)

run :: [Int] -> [Int] -> Either String [Cell]
run i p = traverse parseCell . chunks 3 $ interpretOut p i

solve1 :: [Int] -> Either String Int
solve1 = fmap (length . filter ((== Just Block) . cellTile)) . run []

solutions :: IO ()
solutions = do
  i <- parseProgram "inputs/day13"
  print $ solve1 i
  print $ autoPlay (i & ix 0 .~ 2) ^. score
