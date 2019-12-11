{-# LANGUAGE RecursiveDo #-}

module Day11 where

import Data.Map (Map)
import qualified Data.Map as Map
import IntCode (interpret, ICState(..), parseProgram, parseProgram', Program)
import Control.Monad.Except (MonadError, liftEither, throwError, runExcept)
import Control.Monad.State.Lazy (MonadState, get, modify, execStateT)
import Control.Monad.Fix (MonadFix)
import Data.Functor ((<&>))
import Util (enumIndex)
import Data.List (unfoldr)
import Debug.Trace (traceShow)
import Data.Either (fromRight)
import Data.Text (Text)
import qualified Data.Text.IO as T
import Data.Semigroup (Arg(..))
import Data.Bifunctor (bimap)

data Direction = N | E | S | W deriving (Show, Eq, Enum, Bounded)

data Turn = L | R

data Robot = Robot 
  { robotDirection :: Direction 
  , robotPosition  :: (Int, Int) 
  , robotShipMap   :: Map (Int, Int) Int
  } deriving Show

interpretOut :: Program -> [Int] -> [Int]
interpretOut p i = fromRight (error "interpretOut") (interpret p i <&> reverse . output)

paint :: Int -> Robot -> Robot
paint n (Robot h p m) | n == 0 || n == 1 = Robot h p (Map.insert p n m)
                      | otherwise        = error $ "Unknown color " <> show n

turn :: Int -> Robot -> Robot
turn n (Robot h p m) = Robot (applyTurn (parseTurn n) h) p m
  where
    applyTurn L h = enumIndex $ fromEnum h - 1
    applyTurn R h = enumIndex $ fromEnum h + 1

    parseTurn 0 = L
    parseTurn 1 = R
    parseTurn n = error $ "Unknown turn direction " <> show n

advance :: Robot -> Robot
advance (Robot h p m) = Robot h (go h p) m
  where
    go d (x, y) = case d of
      N -> (x, y + 1)
      E -> (x + 1, y)
      S -> (x, y - 1)
      W -> (x - 1, y)

currentColor :: Robot -> Int
currentColor (Robot _ p m) = Map.findWithDefault 0 p m

paintShip :: Int -> Program -> Robot
paintShip s p =
  let r = interpretOut p (s : fmap fst n)
      n = unfoldr go (r, Robot N (0, 0) mempty)
  in snd $ last n
  where
    go ([], _) = Nothing
    go ([a], _) = error "Unexpected output"
    go ((a:b:rs), r) = 
      let r' = update r 
      in Just ((currentColor r', r'), (rs, r'))
      where
        update = advance . turn b . paint a

solve1 :: Program -> Int
solve1 = length . robotShipMap . paintShip 0

solve2 :: Program -> Text
solve2 = render . robotShipMap . paintShip 1

render :: Map (Int, Int) Int -> Text
render m =
  let aks f = Map.mapKeys (uncurry Arg . f)
      (minx, maxx) = fstA (Map.findMin $ aks id m, Map.findMax $ aks id m)
      (miny, maxy) = fstA (Map.findMin $ aks swap m, Map.findMax $ aks swap m)
  in foldMap (\ps -> foldMap go ps <> "\n") [[(x, y) | x <- [minx..maxx]] | y <- reverse [miny..maxy]]
  where
    go p = case Map.findWithDefault 0 p m of
      1 -> "x"
      _ -> " "
    fstA = bimap unA unA where unA ((Arg a _), _) = a
    swap (a, b) = (b, a)

solutions :: IO ()
solutions = do
  Right i <- parseProgram "inputs/day11"
  print $ solve1 i
  T.putStrLn $ solve2 i