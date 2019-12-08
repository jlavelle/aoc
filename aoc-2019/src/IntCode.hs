module IntCode where

import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Align (alignWith)
import Data.These (these)
import Lens.Micro ((&), _1, (%~), (<&>))
import Util (digits)
import Data.Bool (bool)
import qualified Data.Text as T
import qualified Data.Text.Read as T
import qualified Data.Text.IO as T

data Mode = Immediate | Position
  deriving Show

data Param = Param Mode Int
  deriving Show

data OpCode
  = Add
  | Mul
  | Input
  | Output
  | Halt
  | JumpIfTrue
  | JumpIfFalse
  | LessThan
  | Equals
  deriving Show

data Op = Op OpCode [Mode]
  deriving Show

data ICState = ICState
  { input    :: [Int]
  , output   :: [Int]
  , memory   :: Seq Int
  , pointer  :: Int
  }

newtype Program = Program [Int]

parseMode :: Int -> Either String Mode
parseMode 0 = Right Position
parseMode 1 = Right Immediate
parseMode n = Left $ "Unknown mode " <> show n

parseOp :: Int -> Either String Op
parseOp i =
  let ds = digits i
      (ems, oc) = splitAt (length ds - 2) ds & _1 %~ traverse parseMode . reverse
      modes n = ems <&> \ms -> alignWith (these id id const) ms (replicate n Position)
  in case (dropWhile (== 0) oc) of
    [1] -> Op Add <$> modes 3
    [2] -> Op Mul <$> modes 3
    [3] -> Op Input <$> modes 1
    [4] -> Op Output <$> modes 1
    [5] -> Op JumpIfTrue <$> modes 2
    [6] -> Op JumpIfFalse <$> modes 2
    [7] -> Op LessThan <$> modes 3
    [8] -> Op Equals <$> modes 3
    [9,9] -> Right $ Op Halt []
    _ -> Left $ "Unknown op code " <> show ds

parseProgram :: FilePath -> IO (Either String Program)
parseProgram = fmap parseProgram' . T.readFile

parseProgram' :: T.Text -> Either String Program
parseProgram' = fmap Program . traverse parseInt . T.splitOn ","
  where
    parseInt = fmap fst . T.signed T.decimal

movePointer :: (Int -> Int) -> ICState -> ICState
movePointer f ics = ics { pointer = f $ pointer ics }

interpret :: Program -> [Int] -> Either String ICState
interpret (Program is) h =
  let mem = Seq.fromList is
  in step $ ICState h [] mem 0

step :: ICState -> Either String ICState
step s@(ICState _ _ m p) = do
  Op c ms <- parseOp $ Seq.index m p
  let pvs = Seq.index m <$> [p + 1..p + length ms]
  let params = zipWith Param ms pvs
  case c of
    Halt -> pure s
    _    -> do
      s' <- handleOp c params s
      step s'

handleOp :: OpCode -> [Param] -> ICState -> Either String ICState
handleOp op ps (ICState i o m p) = case (op, ps) of
  (Add, [a, b, c]) -> update (+) a b c
  (Mul, [a, b, c]) -> update (*) a b c
  (Input, [Param Position x]) -> do
    let v = head i
    let m' = Seq.update x v m
    pure $ ICState (tail i) o m' (p + 2)
  (Output, [a]) -> let v = handleParam a m in Right $ ICState i (v:o) m (p + 2)
  (JumpIfTrue, [a, b]) -> jump True a b
  (JumpIfFalse, [a, b]) -> jump False a b
  (LessThan, [a, b, c]) -> update (\x y -> bool 0 1 $ x < y) a b c
  (Equals, [a, b, c]) -> update (\x y -> bool 0 1 $ x == y) a b c
  (Halt, _) -> Left "Halted"
  (x, xs) -> Left $ "Invalid params for op " <> show x <>": " <> show xs
  where
    update f a b (Param Position c) = do
      let av = handleParam a m
      let bv = handleParam b m
      let m' = Seq.update c (f av bv) m
      pure $ ICState i o m' (p + 4)
    update _ _ _ (Param Immediate _) = Left "Unexpected Immediate param in update"

    jump t a b = do
      let av = handleParam a m
      let bv = handleParam b m
      let p' = bool (p + 3) bv $ bool (== 0) (/= 0) t av
      pure $ ICState i o m p'

handleParam :: Param -> Seq Int -> Int
handleParam (Param Immediate x) _ = x
handleParam (Param Position x) m = Seq.index m x
