module Day7 where

import IntCode (Program, interpret, ICState(..), parseProgram, parseProgram')
import Data.Functor ((<&>))
import Control.Foldl (EndoM(..))
import Data.List (permutations)
import Data.Profunctor (lmap)
import Control.Monad.Fix (mfix)

mkPipe :: Program -> [Int] -> Either String [Int]
mkPipe p i = interpret p i <&> reverse . output

programs :: [Int] -> Program -> [[Int] -> Either String [Int]]
programs ps prog = fmap (appEndoM . foldMap go) (permutations ps)
  where
    go i = EndoM $ lmap (i:) $ mkPipe prog

maxAmp :: [[Int] -> Either String [Int]] -> Either String Int
maxAmp = fmap maximum . traverse (fmap last . ($ [0]))

solve1 :: Program -> Either String Int
solve1 = maxAmp . programs [0..4]

solve2 :: Program -> Either String Int
solve2 = maxAmp . fmap loopPipe . programs [5..9]
  where
    loopPipe p i = mfix \out -> p (i <> out)

test1 :: Program
test1 =
  let (Right p) = parseProgram' "3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0"
  in p

test2 :: Program
test2 =
  let (Right p) = parseProgram' "3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5"
  in p

solutions :: IO ()
solutions = do
  i <- parseProgram "inputs/day7"
  print $ solve1 =<< i
  print $ solve2 =<< i
