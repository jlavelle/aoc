{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}

module Day17 where

import IntCode (parseProgram, interpretOut, interpretM, Interpret(..))
import Linear.V2 (V2(..), _x, _y)
import qualified Data.Map as Map
import Data.Map (Map)
import Control.Lens ((^.), ix, (.~), (&))
import Control.Monad.State (StateT, evalStateT, get, put)
import Control.Monad.IO.Class (liftIO)
import Data.Witherable (mapMaybe)
import qualified Algebra.Graph.AdjacencyMap as AM
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Applicative ((<|>), liftA2)
import Data.List (unfoldr, intersperse)
import Util (eqOn, parseGrid, mkGraph2D, neighbors2D, Node(..), position)
import Control.Monad (guard)
import Data.Functor (($>))

data Direction = N | E | S | W
  deriving (Eq, Ord, Enum, Bounded, Show)

data Tile
  = Robot Direction
  | Scaffold
  deriving (Eq, Ord, Show)

data Turn = L | R
  deriving (Eq, Show)

data Instr = Turn Turn | Move Int
  deriving Show

renderInstr :: Instr -> String
renderInstr = \case
  Turn t -> case t of
    L -> "L"
    R -> "R"
  Move i -> show i

renderInstrs :: [Instr] -> String
renderInstrs = concat . intersperse "," . fmap renderInstr

renderTile :: Tile -> Char
renderTile = \case
  Robot d -> case d of
    N -> '^'
    E -> '>'
    S -> 'v'
    W -> '<'
  Scaffold -> '#'

-- Everything is simpler if we don't bother with the tumbling state
parseTile :: Char -> Maybe Tile
parseTile = \case
  '^' -> Just $ Robot N
  'v' -> Just $ Robot S
  '>' -> Just $ Robot E
  '<' -> Just $ Robot W
  '#' -> Just Scaffold
  '.' -> Nothing
  'X' -> error $ "The robot is tumbling D:"
  c   -> error $ "Unknown tile " <> show c

runASCII :: [Int] -> Map (V2 Int) Tile
runASCII = parseGrid parseTile . lines . fmap toEnum . flip interpretOut []

-- Run the program interactively with optional input
asciiIO :: [Int] -> StateT String IO ()
asciiIO = interpretM Interpret{..}
  where
    continue = const $ pure ()
    input    = get >>= \case
      []     -> fromEnum <$> liftIO getChar
      (x:xs) -> put xs $> fromEnum x
    output a | a > 255   = liftIO $ putStr $ show a
             | otherwise = liftIO $ putChar $ toEnum a
    halted   = liftIO (putStr "\n" *> putStrLn "Done.") *> pure ()
    err e    = error $ show e

intersections :: Map (V2 Int) Tile -> [V2 Int]
intersections m = filter (`isIntersection` m) $ Map.keys m

isIntersection :: V2 Int -> Map (V2 Int) Tile -> Bool
isIntersection p m = length (mapMaybe (`Map.lookup` m) $ neighbors2D p) == 4

-- Ends are only adjacent to one other node
endpoints :: Foldable f => Map k (f a) -> (k, k)
endpoints am =
  let [a, b] = Map.keys $ Map.filter ((== 1) . length) am
  in (a, b)

straight :: [Node Tile] -> [Node Tile] -> Maybe (Node Tile)
straight (a:b:_) s =
  let same | eqOn (position . _x) a b = Just (position . _x)
           | eqOn (position . _y) a b = Just (position . _y)
           | otherwise = Nothing
  in same >>= \l -> foldr (go l) Nothing s
  where
    go l x r | x ^. l == a ^. l = Just x
             | otherwise = r
straight _ _ = Nothing

onlyOne :: [Node Tile] -> Maybe (Node Tile)
onlyOne [x] = Just x
onlyOne _   = Nothing

tour :: Map (V2 Int) Tile -> [Instr]
tour m = foldr alg [] $ unfoldr coalg (dir, [start])
  where
    alg (Move n) (Move n' : acc) = Move (n + n') : acc
    alg x acc = x : acc

    coalg (_, []) = Nothing
    coalg (d, mv@(a:_))
      | a == end  = Nothing
      | otherwise =
          let moves  = getMoves a mv
              Just p = straight mv moves <|> onlyOne moves
          in case getTurn d (a ^. position) (p ^. position) of
            Just (t, d') -> Just (Turn t, (d', mv))
            Nothing      -> Just (Move 1, (d, p:mv))

    am :: Map (Node Tile) (Set (Node Tile))
    am = AM.adjacencyMap $ mkGraph2D m

    Just (Robot dir) = Map.lookup (start ^. position) m

    (start, end) =
      let (ea, eb) = endpoints am
      in case Map.lookup (ea ^. position) m of
        Just (Robot _) -> (ea, eb)
        _ -> (eb, ea)

    getMoves x mv = filter (not . flip elem (take 2 mv))
                  $ Set.toList
                  $ Map.findWithDefault Set.empty x am

getTurn :: Ord a => Direction -> V2 a -> V2 a -> Maybe (Turn, Direction)
getTurn d a b = directionTo a b >>= \d' -> guard (d /= d') *> pure (turnTo d d', d')

directionTo :: Ord a => V2 a -> V2 a -> Maybe Direction
directionTo a b = case liftA2 compare a b of
  V2 EQ GT -> Just N
  V2 EQ LT -> Just S
  V2 LT EQ -> Just E
  V2 GT EQ -> Just W
  _        -> Nothing

turnTo :: Direction -> Direction -> Turn
turnTo a b = case (a, b) of
  (N, E) -> R
  (N, W) -> L
  (S, W) -> R
  (S, E) -> L
  (W, N) -> R
  (W, S) -> L
  (E, S) -> R
  (E, N) -> L
  _ -> error "Cannot turn this way"

solve1 :: [Int] -> Int
solve1 = sum . fmap product . intersections . runASCII

solutions :: IO ()
solutions = do
  i <- parseProgram "inputs/day17"
  let r = runASCII i
  putStrLn $ renderInstrs $ tour r
  evalStateT (asciiIO $ i & ix 0 .~ 2) solution

isRobot :: Tile -> Bool
isRobot (Robot _) = True
isRobot _ = False

-- TODO Compute solution
solution :: String
solution = "A,B,A,C,A,A,C,B,C,B\n" <> foldMap (\x -> x <> "\n") [a, b, c, "n"]
  where
    a = renderInstrs [Turn L, Move 12, Turn L, Move 8, Turn R, Move 12]
    b = renderInstrs [Turn L, Move 10, Turn L, Move 8, Turn L, Move 12, Turn R, Move 12]
    c = renderInstrs [Turn R, Move 12, Turn L, Move 8, Turn L, Move 10]
