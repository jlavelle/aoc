module Day7 where

import IntCode (Program, interpretP, parseProgram, parseProgram')
import Data.Functor ((<&>))
import Control.Foldl (EndoM(..))
import Data.List (permutations)

mkPipe :: Program -> [Int] -> Either String [Int]
mkPipe p i = interpretP p i <&> fst

programs :: Program -> [[Int] -> Either String [Int]]
programs prog = fmap (appEndoM . foldMap go) (permutations [0..4])
  where
    go p = EndoM $ addPhase p $ mkPipe prog
    addPhase i f = f . (i:)

solve1 :: Program -> Either String Int
solve1 = fmap maximum . traverse (fmap head . ($ [0])) . programs

test1 :: Program
test1 =
  let (Right p) = parseProgram' "3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0"
  in p

solutions :: IO ()
solutions = do
  i <- parseProgram "inputs/day7"
  print $ solve1 =<< i
