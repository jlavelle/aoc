{-# LANGUAGE RecursiveDo #-}

module Day11 where

import Data.Map (Map)
import qualified Data.Map as Map
import IntCode2 (parseProgram, initICState, compile, IntCode(..))
import Util (enumIndex)
import Data.Text (Text)
import qualified Data.Text.IO as T
import Linear.V2 (V2(..), _x, _y)
import Control.Lens ((+~), (-~), (^.), (&))
import qualified Control.Foldl as F
import Data.Ord (comparing)
import Data.Bool (bool)
import Data.Maybe (fromJust)

data Direction = N | E | S | W deriving (Show, Eq, Enum, Bounded)

data Turn = L | R

data Robot = Robot 
  { robotDirection :: Direction 
  , robotPosition  :: V2 Int
  , robotShipMap   :: Map (V2 Int) Int
  } deriving Show

paint :: Int -> Robot -> Robot
paint n (Robot h p m) | n == 0 || n == 1 = Robot h p (Map.insert p n m)
                      | otherwise        = error $ "Unknown color " <> show n

turn :: Int -> Robot -> Robot
turn n (Robot h p m) = Robot (applyTurn (parseTurn n) h) p m
  where
    applyTurn L a = enumIndex $ fromEnum a - 1
    applyTurn R a = enumIndex $ fromEnum a + 1

    parseTurn 0 = L
    parseTurn 1 = R
    parseTurn x = error $ "Unknown turn direction " <> show x

advance :: Robot -> Robot
advance (Robot h p m) = Robot h (go h p) m
  where
    go d = case d of
      N -> _y +~ 1
      E -> _x +~ 1
      S -> _y -~ 1
      W -> _x -~ 1

currentColor :: Robot -> Int
currentColor (Robot _ p m) = Map.findWithDefault 0 p m

paintShip :: Int -> [Int] -> Robot
paintShip s p = go (Robot N (V2 0 0) mempty) (Just s) $ compile $ initICState p
  where
    go r i x = case x of
      Error e -> error $ show e
      Halted  -> r
      Input f | Just i' <- i -> go r Nothing $ f i'
              | otherwise    -> error "Out of input"
      Output c (Output t next) ->
        let r' = advance . turn t . paint c $ r
        in go r' (Just $ currentColor r') next
      _ -> error "Unexpected step"

solve1 :: [Int] -> Int
solve1 = length . robotShipMap . paintShip 0

solve2 :: [Int] -> Text
solve2 = render . robotShipMap . paintShip 1

render :: Map (V2 Int) Int -> Text
render m = foldMap (\ps -> foldMap go ps <> "\n") [[V2 x y | x <- xr] | y <- yr]
  where
    go p = Map.findWithDefault " " p $ fmap (bool " " "x" . (== 1)) m
    xr = [minx..maxx]
    yr = reverse [miny..maxy]
    (minx, maxx, miny, maxy) = Map.keys m & F.fold mms
      where
        mms = (,,,) <$> minV _x <*> maxV _x <*> minV _y <*> maxV _y
        minV = comp F.minimumBy
        maxV = comp F.maximumBy
        comp f l = fmap ((^. l) . fromJust) $ f $ comparing (^. l)

solutions :: IO ()
solutions = do
  i <- parseProgram "inputs/day11"
  print $ solve1 i
  T.putStrLn $ solve2 i
