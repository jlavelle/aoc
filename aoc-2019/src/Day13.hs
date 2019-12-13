module Day13 where

import IntCode (parseProgram', parseProgram, ICState(..), interpret, Program, setAddr)
import Data.Functor ((<&>))
import Util (chunks)
import Linear.V2 (V2(..))

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

cellTile :: Cell -> Maybe Tile
cellTile (TileCell t _) = Just t
cellTile _ = Nothing

parseCell :: [Int] -> Either String Cell
parseCell [a, b, c] = case (a, b) of
  (-1, 0) -> Right $ Score c
  _       -> Right $ TileCell (toEnum c) (V2 a b)
parseCell _ = Left "Unexpected number of arguments"

run :: [Int] -> Program -> Either String [Cell]
run i p = interpret p i >>= traverse parseCell . chunks 3 . reverse . output

solve1 :: Program -> Either String Int
solve1 = fmap (length . filter ((== Just Block) . cellTile)) . run []

solutions :: IO ()
solutions = do
  Right i <- parseProgram "inputs/day13"
  print $ solve1 i
