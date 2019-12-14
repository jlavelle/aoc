{-# LANGUAGE TemplateHaskell #-}

module IntCode2 where

import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.Trans (lift)
import Control.Monad.State (MonadState, StateT, evalStateT, runStateT, execStateT, get, modify)
import Control.Monad.Except (MonadError, throwError, catchError, runExcept)
import Control.Monad.Free.Class (MonadFree, liftF)
import Control.Monad.Trans.Free (FreeT, iterTM)
import Control.Monad.Free.TH (makeFree)
import Control.Lens.TH (makeLenses)
import Control.Lens (use, ix, (^?), (+=), (%=), (.=), _1, _2, zoom)
import Control.Monad (unless)
import Data.Maybe (fromMaybe)
import Util (digits)
import Data.Bool (bool)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import qualified Data.Text.Read as T
import qualified Data.Text.IO as T

data CoroutineF i o r
  = Await (i -> r)
  | Yield o r
  deriving Functor

makeFree ''CoroutineF

type Stream i o m a = FreeT (CoroutineF i o) m a

data Mode
  = Immediate
  | Position
  | Relative
  deriving (Show, Eq, Ord)

data Param a = Param !Mode !a
  deriving (Functor, Foldable, Traversable, Show, Eq, Ord)

data Op a
  = Add   !a !a !a
  | Mul   !a !a !a
  | Inp   !a
  | Out   !a
  | JumpT !a !a
  | JumpF !a !a
  | OpLT  !a !a !a
  | OpEQ  !a !a !a
  | Offs  !a
  | Halt
  deriving (Functor, Foldable, Traversable, Show, Eq, Ord)

data ICState k a = ICState
  { _memory   :: Map k a
  , _pointer  :: !k
  , _relative :: !k
  } deriving Show

makeLenses ''ICState

newtype Program a = Program [a]

data Error a
  = UnknownOpCode a
  | UnknownParamMode a
  | SegFault a
  | ImmediateModeWrite
  | OutOfInput
  deriving Show

type MonadIC m a = (Integral a, Ord a, MonadState (ICState a a) m, MonadError (Error a) m)

deref :: MonadIC m a => a -> m a
deref ptr = do
  mem <- use memory
  case Map.lookup ptr mem of
    Just x  -> pure x
    Nothing -> throwError $ SegFault ptr

derefDefault :: MonadIC m a => a -> a -> m a
derefDefault def ptr = deref ptr `catchError` go
  where
    go = \case
      SegFault _ -> pure def
      e -> throwError e

derefCurrent :: MonadIC m a => m a
derefCurrent = use pointer >>= deref

writeMem :: MonadIC m a => a -> a -> m ()
writeMem p a = memory %= Map.insert p a

parseCurrentOp :: forall m a. MonadIC m a => m (Op (Param a))
parseCurrentOp = derefCurrent >>= go 
  where
    go :: a -> m (Op (Param a))
    go (flip quotRem 100 -> (reverse . digits -> pcs, oc)) = flip evalStateT 1 $ case oc of
      1  -> Add   <$> param <*> param <*> param
      2  -> Mul   <$> param <*> param <*> param
      3  -> Inp   <$> param
      4  -> Out   <$> param
      5  -> JumpT <$> param <*> param
      6  -> JumpF <$> param <*> param
      7  -> OpLT  <$> param <*> param <*> param
      8  -> OpEQ  <$> param <*> param <*> param
      9  -> Offs  <$> param
      99 -> pure Halt
      _  -> throwError $ UnknownOpCode oc
      where
        param = do
          n <- get <* modify succ
          let pm = parseParamMode $ fromMaybe 0 $ pcs ^? ix (fromIntegral n - 1)
          ptr <- lift $ use pointer
          pv  <- lift $ derefDefault 0 $ ptr + n
          case pm of
            Right m -> pure $ Param m pv
            Left n  -> throwError $ UnknownParamMode n
        
        parseParamMode = \case
          0 -> Right Position
          1 -> Right Immediate
          2 -> Right Relative
          n -> Left n

runIC :: MonadIC m a => Stream a a m ()
runIC = do
  op <- parseCurrentOp
  case op of
    Halt -> pure ()
    _    -> handleOp op *> runIC

handleOp :: MonadIC m a => Op (Param a) -> Stream a a m ()
handleOp op = runOp *> advancePtr
  where
    runOp = case op of
      Add a b c  -> binOp (+) a b c
      Mul a b c  -> binOp (*) a b c
      Inp a      -> await >>= writeParam a
      Out a      -> readParam a >>= yield
      JumpT a b  -> jump (opLength op) True a b
      JumpF a b  -> jump (opLength op) False a b
      OpLT a b c -> binOp (\x y -> bool 0 1 $ x < y) a b c
      OpEQ a b c -> binOp (\x y -> bool 0 1 $ x == y) a b c
      Offs a     -> readParam a >>= (relative +=)
      Halt       -> pure ()

    advancePtr = unless (isJump op) $ pointer += opLength op

opLength :: Integral b => Op a -> b
opLength = (+ 1) . fromIntegral . length

binOp :: MonadIC m a => (a -> a -> a) -> Param a -> Param a -> Param a -> m ()
binOp f a b c = do
  av <- readParam a
  bv <- readParam b
  writeParam c (f av bv)

writeParam :: MonadIC m a => Param a -> a -> m ()
writeParam (Param m a) x = case m of
  Relative -> use relative >>= \rp -> writeMem (rp + a) x
  Position -> writeMem a x
  _ -> throwError ImmediateModeWrite

readParam :: MonadIC m a => Param a -> m a
readParam (Param m a) = case m of
  Relative  -> use relative >>= \rp -> derefDefault 0 (rp + a)
  Position  -> derefDefault 0 a
  Immediate -> pure a

jump :: MonadIC m a => a -> Bool -> Param a -> Param a -> m ()
jump opLen cond a b = do
  av <- readParam a
  bv <- readParam b
  let r = bool (== 0) (/= 0) cond av
  bool (pointer += opLen) (pointer .= bv) r

isJump :: Op a -> Bool
isJump (JumpT _ _) = True
isJump (JumpF _ _) = True
isJump _ = False

interpretP :: (Integral a, Ord a) => Program a -> [a] -> Either (Error a) (Seq a, ICState a a)
interpretP (Program p) i = 
  let mem  = Map.fromAscList $ zip [0..] p
      init = ICState mem 0 0
  in runExcept . flip runStateT init . runICStreamP i $ runIC

runICStreamP :: forall m a. MonadIC m a => [a] -> Stream a a m () -> m (Seq a)
runICStreamP i = fmap snd . flip execStateT (i, Seq.empty) . iterTM go
  where
    go :: CoroutineF a a (StateT ([a], Seq a) m ()) -> StateT ([a], Seq a) m ()
    go (Yield a r) = (_2 %= snoc a) *> r
    go (Await f) = do
      (a, r) <- unconsE =<< use _1
      _1 .= r
      f a

snoc :: a -> Seq a -> Seq a
snoc a s = s Seq.|> a

unconsE :: MonadError (Error x) m => [x] -> m (x, [x])
unconsE [] = throwError OutOfInput
unconsE (x:xs) = pure (x, xs)

parseProgram :: Integral a => FilePath -> IO (Either String (Program a))
parseProgram = fmap parseProgram' . T.readFile

parseProgram' :: Integral a => T.Text -> Either String (Program a)
parseProgram' = fmap Program . traverse parseInt . T.splitOn ","
  where
    parseInt = fmap fst . T.signed T.decimal