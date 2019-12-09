module IntCode where

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Align (alignWith)
import Data.These (these)
import Lens.Micro ((&), _1, (%~), (<&>))
import Util (digits)
import Data.Bool (bool)
import qualified Data.Text as T
import qualified Data.Text.Read as T
import qualified Data.Text.IO as T

data Mode
  = Immediate
  | Position
  | Relative
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
  | OffsetRB
  deriving Show

data Op = Op OpCode [Mode]
  deriving Show

data ICState = ICState
  { input    :: [Int]
  , output   :: [Int]
  , memory   :: IntMap Int
  , pointer  :: Int
  , relative :: Int
  }

newtype Program = Program [Int]

parseMode :: Int -> Either String Mode
parseMode 0 = Right Position
parseMode 1 = Right Immediate
parseMode 2 = Right Relative
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
    [9] -> Op OffsetRB <$> modes 1
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

initICState :: ICState
initICState = ICState [] [] mempty 0 0

interpret :: Program -> [Int] -> Either String ICState
interpret (Program is) h =
  let mem = IntMap.fromAscList $ zip [0..] is
  in step $ initICState { memory = mem, input = h }

step :: ICState -> Either String ICState
step s@(ICState _ _ m p _) = do
  Op c ms <- parseOp =<< lookupE p m
  let pvs = flip (IntMap.findWithDefault 0) m <$> [p + 1..p + length ms]
  let params = zipWith Param ms pvs
  case c of
    Halt -> pure s
    _    -> do
      s' <- handleOp c params s
      step s'

handleOp :: OpCode -> [Param] -> ICState -> Either String ICState
handleOp op ps (ICState i o m p rb) = case (op, ps) of
  (Add, [a, b, c]) -> update (+) a b c
  (Mul, [a, b, c]) -> update (*) a b c
  (Input, [Param mode x]) -> do
    let v = head i
    pos <- case mode of
      Relative -> Right $ x + rb
      Position -> Right x
      _        -> Left "Invalid input mode"
    let m' = IntMap.insert pos v m
    pure $ ICState (tail i) o m' (p + 2) rb
  (Output, [a]) -> let v = handleParam a rb m in Right $ ICState i (v:o) m (p + 2) rb
  (JumpIfTrue, [a, b]) -> jump True a b
  (JumpIfFalse, [a, b]) -> jump False a b
  (LessThan, [a, b, c]) -> update (\x y -> bool 0 1 $ x < y) a b c
  (Equals, [a, b, c]) -> update (\x y -> bool 0 1 $ x == y) a b c
  (OffsetRB, [a]) -> pure $ ICState i o m (p + 2) (rb + handleParam a rb m)
  (Halt, _) -> Left "Halted"
  (x, xs) -> Left $ "Invalid params for op " <> show x <>": " <> show xs
  where
    update f a b (Param mode c) = do
      let av = handleParam a rb m
      let bv = handleParam b rb m
      c' <- case mode of
        Relative -> Right $ c + rb
        Position -> Right c
        _        -> Left "Invalid update mode"
      let m' = IntMap.insert c' (f av bv) m
      pure $ ICState i o m' (p + 4) rb

    jump t a b = do
      let av = handleParam a rb m
      let bv = handleParam b rb m
      let p' = bool (p + 3) bv $ bool (== 0) (/= 0) t av
      pure $ ICState i o m p' rb

handleParam :: Param -> Int -> IntMap Int -> Int
handleParam (Param Immediate x) _ _ = x
handleParam (Param Position x) _ m = IntMap.findWithDefault 0 x m
handleParam (Param Relative x) rb m = IntMap.findWithDefault 0 (rb + x) m

lookupE :: Int -> IntMap a -> Either String a
lookupE x m = maybe (Left $ "Key " <> show x <> " not found") Right $ IntMap.lookup x m
