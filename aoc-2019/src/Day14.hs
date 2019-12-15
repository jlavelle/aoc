{-# LANGUAGE TemplateHaskell #-}

module Day14 where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Monad.State (MonadState, StateT, get, modify, runStateT)
import Control.Monad.Reader (MonadReader, ReaderT, ask, runReaderT)
import Control.Monad.Except (MonadError, throwError, runExcept, Except)
import Text.Megaparsec.Char (space1, upperChar, char)
import Text.Megaparsec (some, manyTill, Parsec, empty, optional, eof, parse)
import Data.Void (Void)
import Control.Lens.TH (makeLenses)
import Control.Lens ((^.), (*~), each, (&), (<&>), (.~))
import Data.Bool (bool)
import Data.Tree (Tree)
import qualified Data.Tree as Tree

data Chemical = Chemical
  { _amount :: Int
  , _name   :: Text
  } deriving (Eq, Ord, Show)

makeLenses ''Chemical

data Formula = Formula
  { _inputs :: [Chemical]
  , _output :: Chemical
  } deriving (Eq, Ord, Show)

makeLenses ''Formula

mkDatabase :: [Formula] -> Map Text Formula
mkDatabase = foldr go mempty
  where
    go x a = Map.insert (x ^. output . name) x a

type Chem m = (MonadState (Map Text Int) m, MonadReader (Map Text Formula) m, MonadError Text m)

type ChemM a = ReaderT (Map Text Formula) (StateT (Map Text Int) (Except Text)) a

runChemM :: Map Text Formula -> ChemM a -> Either Text a
runChemM d = runExcept . fmap fst . flip runStateT mempty . flip runReaderT d

isOre :: Chemical -> Bool
isOre x = x ^. name == "ORE"

prodTree :: Chem m => Chemical -> m (Tree Chemical)
prodTree c = Tree.unfoldTreeM go c
  where
    go x = order x <&> \cs -> (x, cs)

oreRequired :: Tree Chemical -> Int
oreRequired = Tree.foldTree go
  where
    go x@(isOre -> True) [] = x ^. amount
    go _ as = sum as

order :: Chem m => Chemical -> m [Chemical]
order (isOre -> True) = pure []
order c = do
  f <- ask >>= lookupE (c ^. name)
  cached <- useCache c
  let req  = c ^. amount - cached
  case req of
    0 -> pure []
    _ -> do
      let prod = f ^. output ^. amount
          mult = let m = runMult req prod in bool m 1 $ m == 0
          ins  = f ^. inputs & each . amount *~ mult
      addToCache (c ^. name) $ prod * mult - req
      pure ins

runMult :: Int -> Int -> Int
runMult req prod = ceiling (fromIntegral req / fromIntegral prod :: Double)

useCache :: Chem m => Chemical -> m Int
useCache c = do
  cached <- get <&> Map.findWithDefault 0 (c ^. name)
  let t = c ^. amount - cached
      r | t >= 0    = cached
        | otherwise = c ^. amount
  modify $ Map.adjust (subtract r) (c ^. name)
  pure r

addToCache :: Chem m => Text -> Int -> m ()
addToCache t n = modify $ Map.insertWith (+) t n

lookupE :: MonadError Text m => Text -> Map Text a -> m a
lookupE t = maybe (throwError $ "Key " <> t <> " not found.") pure . Map.lookup t

renderTree :: Tree Chemical -> String
renderTree = Tree.drawTree . fmap renderChem
  where
    renderChem c = show (c ^. amount) <> " " <> T.unpack (c ^. name)

type Parser = Parsec Void Text

sc :: Parser ()
sc = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

number :: Integral a => Parser a
number = lexeme L.decimal

mcomma :: Parser (Maybe Char)
mcomma = optional $ lexeme $ char ','

identifier :: Parser Text
identifier = T.pack <$> lexeme (some upperChar)

parseChemical :: Parser Chemical
parseChemical = Chemical <$> number <*> identifier

parseFormula :: Parser Formula
parseFormula = Formula <$> chems <*> parseChemical
  where
    chems = manyTill (parseChemical <* mcomma) (symbol "=>")

parseFormulas :: Parser [Formula]
parseFormulas = manyTill parseFormula eof

test1 :: Map Text Formula
test1 = mkDatabase i
  where
    Right i = parse parseFormulas "" inp
    inp = "10 ORE => 10 A\n1 ORE => 1 B\n7 A, 1 B => 1 C\n7 A, 1 C => 1 D\n7 A, 1 D => 1 E\n7 A, 1 E => 1 FUEL"

test2 :: Map Text Formula
test2 = mkDatabase i
  where
    Right i = parse parseFormulas "" inp
    inp = "9 ORE => 2 A\n8 ORE => 3 B\n7 ORE => 5 C\n3 A, 4 B => 1 AB\n5 B, 7 C => 1 BC\n4 C, 1 A => 1 CA\n2 AB, 3 BC, 4 CA => 1 FUEL"

fuel :: Chemical
fuel = Chemical 1 "FUEL"

solve1 :: [Formula] -> Either Text Int
solve1 fs = oreRequired <$> runChemM (mkDatabase fs) (prodTree fuel)

solve2 :: [Formula] -> Either Text Int
solve2 fs = runChemM (mkDatabase fs) go
  where
    go = do
      single <- creq 1
      let est = div target single
      oest <- creq est
      loop oest est est

    loop o f delta
      | o == target = pure f
      | o > target = do
          let nd = div delta 2
              nf = f - nd
          no <- creq nf
          loop no nf nd
      | o < target = do
          p1 <- creq $ f + 1
          if p1 >= target
            then pure f
            else do
              let nf = f + delta
              no <- creq nf
              loop no nf delta

    creq n = (oreRequired <$> prodTree (fuel & amount .~ n)) <* modify (const mempty)
    target = 1000000000000

parseInput :: IO [Formula]
parseInput = do
  Right i <- parse parseFormulas "" <$> T.readFile "inputs/day14"
  pure i

solutions :: IO ()
solutions = do
  i <- parseInput
  print $ solve1 i
  print $ solve2 i

tests :: IO ()
tests = do
  doTest "Test 1" 31 fuel test1
  doTest "Test 2" 165 fuel test2
  where
  doTest n ex x d = do
    putStrLn n
    let Right a = oreRequired <$> runChemM d (prodTree x)
    putStrLn $ "Expected: " <> show ex
    putStrLn $ "Actual: " <>  show a
    putStrLn $ if ex == a then "Passed." else "Failed."
