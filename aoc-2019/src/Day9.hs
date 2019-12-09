module Day9 where

import IntCode (interpret, parseProgram, parseProgram', Program, ICState(..))

solve :: Int -> Program -> Either String Int
solve i p = case output <$> interpret p [i] of
  Right [a] -> Right a
  Right as -> Left $ "Invalid output: " <> show as
  Left e -> Left e

solve1 :: Program -> Either String Int
solve1 = solve 1

solve2 :: Program -> Either String Int
solve2 = solve 2

solutions :: IO ()
solutions = do
  i <- parseProgram "inputs/day9"
  print $ solve1 =<< i
  print $ solve2 =<< i

test1 :: Program
test1 =
  let (Right p) = parseProgram' "1102,34915192,34915192,7,4,7,99,0"
  in p

test2 :: Program
test2 =
  let (Right p) = parseProgram' "109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99"
  in p
