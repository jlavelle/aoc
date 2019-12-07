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
import Pipes (Pipe, (>->))
import qualified Pipes as P
import qualified Pipes.Prelude as P
import Control.Monad.Except (ExceptT, throwError, liftEither, runExceptT)
import Data.Functor.Identity (runIdentity)
import Data.Functor (($>))

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
  { memory  :: (IntMap Int)
  , pointer :: !Int
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

lookupE :: Monad m => Int -> IntMap a -> InterpretM m a
lookupE i m = liftEither
            $ maybe (Left $ "Position " <> show i <> " not found") Right
            $ IntMap.lookup i m

interpretP :: Program -> [Int] -> Either String ([Int], ICState)
interpretP p inp = run $ (P.each inp $> initState p) >-> interpret p
  where
    run = runIdentity . runExceptT . P.toListM'

initState :: Program -> ICState
initState (Program is) = ICState (IntMap.fromAscList $ zip [0..] is) 0

type InterpretM m a = Pipe Int Int (ExceptT String m) a

interpret :: Monad m => Program -> InterpretM m ICState
interpret = step . initState

step :: Monad m => ICState -> InterpretM m ICState
step s@(ICState m p) = do
  Op c ms <- lookupE p m >>= liftEither . parseOp
  pvs <- traverse (flip lookupE m) [p + 1..p + length ms]
  let params = zipWith Param ms pvs
  case c of
    Halt -> pure s
    _    -> do
      s' <- handleOp c params s
      step s'

handleOp :: Monad m => OpCode -> [Param] -> ICState -> InterpretM m ICState
handleOp o ps (ICState m p) = case (o, ps) of
  (Add, [a, b, c]) -> update (+) a b c
  (Mul, [a, b, c]) -> update (*) a b c
  (Input, [Param Position x]) -> do
    v <- P.await
    let m' = IntMap.insert x v m
    pure $ ICState m' (p + 2)
  (Output, [a]) -> do
    v <- handleParam a
    P.yield v
    pure $ ICState m (p + 2)
  (JumpIfTrue, [a, b]) -> jump True a b
  (JumpIfFalse, [a, b]) -> jump False a b
  (LessThan, [a, b, c]) -> update (\x y -> bool 0 1 $ x < y) a b c
  (Equals, [a, b, c]) -> update (\x y -> bool 0 1 $ x == y) a b c
  (Halt, _) -> throwError "Uncaught Halt"
  (x, xs) -> throwError $ "Invalid params for op " <> show x <>": " <> show xs
  where
    update f a b (Param Position c) = do
      av <- handleParam a
      bv <- handleParam b
      let m' = IntMap.insert c (f av bv) m
      pure $ ICState m' (p + 4)
    update _ _ _ (Param Immediate _) = throwError "Unexpected Immediate param in update"

    jump t a b = do
      av <- handleParam a
      bv <- handleParam b
      let p' = bool (p + 3) bv $ bool (== 0) (/= 0) t av
      pure $ ICState m p'

    handleParam (Param Immediate x) = liftEither $ Right x
    handleParam (Param Position x)  = lookupE x m
