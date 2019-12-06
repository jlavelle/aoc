module IntCode where

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Align (alignWith)
import Data.These (these)
import Lens.Micro ((&), _1, (%~), (<&>))
import Util (digits)

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
  deriving Show

data Op = Op OpCode [Mode]
  deriving Show

data Handle = Handle
  { input  :: ![Int]
  , output :: ![Int]
  }

data ICState = ICState
  { handle  :: Handle
  , memory  :: (IntMap Int)
  , pointer :: !Int
  }

parseMode :: Int -> Either String Mode
parseMode 0 = Right Position
parseMode 1 = Right Immediate
parseMode n = Left $ "Unknown mode " <> show n

parseParams :: [Mode] -> [Int] -> Maybe [Param]
parseParams ms xs = sequenceA $ alignWith mkParam (reverse ms) (reverse xs)
  where
    mkParam = these (const Nothing) (Just . Param Position) ((Just .) . Param)

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
    [9,9] -> Right $ Op Halt []
    _ -> Left $ "Unknown op code " <> show ds

defaultHandle :: Handle
defaultHandle = Handle [] []

readInput :: Handle -> Either String (Int, Handle)
readInput (Handle i o) = case i of
  []   -> Left "Out of input"
  x:xs -> Right (x, Handle xs o)

writeOutput :: Int -> Handle -> Handle
writeOutput o (Handle i os) = Handle i (o:os)

movePointer :: (Int -> Int) -> ICState -> ICState
movePointer f ics = ics { pointer = f $ pointer ics }

lookupE :: Int -> IntMap a -> Either String a
lookupE i m = maybe (Left $ "Position " <> show i <> " not found") Right $ IntMap.lookup i m

interpret :: [Int] -> Handle -> Either String ICState
interpret is h =
  let mem = IntMap.fromAscList $ zip [0..] is
  in step $ ICState h mem 0

step :: ICState -> Either String ICState
step s@(ICState h m p) = do
  Op c ms <- lookupE p m >>= parseOp
  pvs <- traverse (flip lookupE m) [p + 1..p + length ms]
  let params = zipWith Param ms pvs
  case c of
    Halt -> pure s
    _    -> do
      (h', m') <- handleOp c params h m
      step $ ICState h' m' $ p + length ms + 1

handleOp :: OpCode -> [Param] -> Handle -> IntMap Int -> Either String (Handle, IntMap Int)
handleOp o ps h m = case (o, ps) of
  (Add, [a, b, c]) -> update (+) a b c m <&> (h,)
  (Mul, [a, b, c]) -> update (*) a b c m <&> (h,)
  (Input, [Param Position x]) -> do
    (v, h') <- readInput h
    let m' = IntMap.insert x v m
    pure (h', m')
  (Output, [a]) -> handleParam a m <&> \ov -> (writeOutput ov h, m)
  (Halt, _) -> Left "Halted"
  (x, xs) -> Left $ "Invalid params for op " <> show x <>": " <> show xs
  where
    update f a b (Param Position c) mem = do
      av <- handleParam a mem
      bv <- handleParam b mem
      pure $ IntMap.insert c (f av bv) mem
    update _ _ _ (Param Immediate _) _ = Left "Unexpected Immediate param in update"

    handleParam (Param Immediate x) _ = Right x
    handleParam (Param Position x) mem = lookupE x mem
