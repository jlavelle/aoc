{-# LANGUAGE RecursiveDo #-}

module Day7 where

import IntCode (Program, interpret, ICState(..), parseProgram, parseProgram')
import Data.Functor ((<&>))
import Control.Foldl (EndoM(..))
import Data.List (permutations)
import Data.Traversable (for)

mkPipe :: Program -> [Int] -> Either String [Int]
mkPipe p i = interpret p i <&> reverse . output

programs :: Program -> [[Int] -> Either String [Int]]
programs prog = fmap (appEndoM . foldMap go) (permutations [0..4])
  where
    go p = EndoM $ addPhase p $ mkPipe prog
    addPhase i f = f . (i:)

solve2 :: Program -> Int
solve2 p = maximum (handlePerm <$> permutations [5..9])
  where
    handlePerm (a:b:c:d:e:_) =
      let (Right o1) = mkPipe p (a:0:o5)
          (Right o2) = mkPipe p (b:o1)
          (Right o3) = mkPipe p (c:o2)
          (Right o4) = mkPipe p (d:o3)
          (Right o5) = mkPipe p (e:o4)
      in o5 !! (length o5 - 1)

solve1 :: Program -> Either String Int
solve1 = fmap maximum . traverse (fmap head . ($ [0])) . programs

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
  print $ solve2 <$> i
