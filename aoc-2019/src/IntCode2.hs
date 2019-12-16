{-# LANGUAGE TemplateHaskell #-}

module IntCode2 where

import Data.Map (Map)
import qualified Data.Map as Map
import Control.Lens.TH (makeLenses)
import Control.Lens (ix, (^?), (^.), (+~), (&), (%~), (.~), (<&>))
import Control.Monad.Except (runExcept, throwError)
import Control.Monad.State.Strict (evalStateT, get, modify)
import Data.Maybe (fromMaybe)
import Util (digits)
import Data.Bool (bool)
import qualified Data.Text as T
import qualified Data.Text.Read as T
import qualified Data.Text.IO as T

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

data ICState a = ICState
  { _memory   :: !(Map a a)
  , _pointer  :: !a
  , _relative :: !a
  } deriving Show

makeLenses ''ICState

data Error a
  = UnknownOpCode a
  | UnknownParamMode a
  | SegFault a
  | OutOfInput
  deriving Show

data IntCode a
  = Input (a -> IntCode a)
  | Output a (IntCode a)
  | Halted
  | Error (Error a)

data IntCodeF a r
  = Continue r
  | InputF (a -> r)
  | OutputF a r
  | HaltedF
  | ErrorF (Error a)
  deriving Functor

interpretOut :: (Show a, Enum a, Ord a, Integral a) => [a] -> [a] -> [a]
interpretOut p i' = go i' $ compile $ initICState p
  where
    go i x = case x of
      Error e -> error $ show e
      Halted  -> []
      Input f | a:as <- i -> go as $ f a
              | otherwise -> error "Out of input"
      Output a r -> a : go i r

interpretSt :: (Show a, Enum a, Ord a, Integral a) => [a] -> [a] -> (ICState a, [a])
interpretSt p i' = fmap reverse $ go i' [] $ initICState p
  where
    go i o s = case step s of
      InputF f | a:as <- i -> go as o $ f a
               | otherwise -> error "Out of input"
      Continue s'  -> go i o s'
      OutputF a s' -> go i (a:o) s'
      HaltedF      -> (s, o)
      ErrorF e     -> error $ show e

initICState :: (Enum a, Ord a, Num a) => [a] -> ICState a
initICState xs = ICState (Map.fromAscList $ zip [0..] xs) 0 0

compile :: (Ord a, Integral a) => ICState a -> IntCode a
compile = compile' step

compile' :: (ICState a -> IntCodeF a (ICState a)) -> ICState a -> IntCode a
compile' f = loop
  where
    loop s = case f s of
      Continue s'  -> loop s'
      InputF g     -> Input (loop . g)
      OutputF a s' -> Output a $ loop s'
      HaltedF      -> Halted
      ErrorF e     -> Error e

step :: (Ord a, Integral a) => ICState a -> IntCodeF a (ICState a)
step s = case parseCurrentOp s of
  Left e   -> ErrorF e
  Right op -> handleOp op <&> advancePtr op
  where
    handleOp op = case op of
      Add a b c  -> Continue $ binOp (+) a b c
      Mul a b c  -> Continue $ binOp (*) a b c
      Inp a      -> InputF $ writeParam a
      Out a      -> OutputF (readParam a) s
      JumpT a b  -> Continue $ jump (opLength op) True a b
      JumpF a b  -> Continue $ jump (opLength op) False a b
      OpLT a b c -> Continue $ binOp (\x y -> bool 0 1 $ x < y) a b c
      OpEQ a b c -> Continue $ binOp (\x y -> bool 0 1 $ x == y) a b c
      Offs a     -> Continue (s & relative +~ readParam a)
      Halt       -> HaltedF
    binOp f a b c = writeParam c $ f (readParam a) (readParam b)
    jump l p a b =
      let r = bool (== 0) (/= 0) p (readParam a)
      in bool (s & pointer +~ l) (s & pointer .~ readParam b) r
    readParam (Param m a) = case m of
      Relative  -> deref' (s ^. relative + a) s
      Position  -> deref' a s
      Immediate -> a
    writeParam (Param m a) x = case m of
      Relative  -> s & memory %~ Map.insert (s ^. relative + a) x
      Position  -> s & memory %~ Map.insert a x
      Immediate -> error "Immediate mode write"
    advancePtr op | isJump op = id
                  | otherwise = pointer +~ opLength op

deref :: Ord a => a -> ICState a -> Either (Error a) a
deref k s = maybe (Left $ SegFault k) pure $ Map.lookup k (s ^. memory)

deref' :: (Num a, Ord a) => a -> ICState a -> a
deref' k s = Map.findWithDefault 0 k (s ^. memory)

parseCurrentOp :: forall a. (Integral a, Ord a) => ICState a -> Either (Error a) (Op (Param a))
parseCurrentOp s = go =<< deref (s ^. pointer) s
  where
    go :: a -> Either (Error a) (Op (Param a))
    go (flip quotRem 100 -> (reverse . digits -> pcs, oc)) = pparse case oc of
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
        pparse = runExcept . flip evalStateT 1
        param = do
          n <- get <* modify succ
          m <- parseParamMode n
          pure $ Param m $ deref' ((s ^. pointer) + n) s
        parseParamMode n = f $ fromMaybe 0 $ pcs ^? ix (fromIntegral n - 1)
          where
            f = \case
              0 -> pure Position
              1 -> pure Immediate
              2 -> pure Relative
              e -> throwError $ UnknownParamMode e

isJump :: Op a -> Bool
isJump (JumpT _ _) = True
isJump (JumpF _ _) = True
isJump _ = False

opLength :: Integral b => Op a -> b
opLength = (+ 1) . fromIntegral . length

parseProgram :: Integral a => FilePath -> IO [a]
parseProgram = fmap parseProgram' . T.readFile

parseProgram' :: Integral a => T.Text -> [a]
parseProgram' = either error id . traverse parseInt . T.splitOn ","
  where
    parseInt = fmap fst . T.signed T.decimal
