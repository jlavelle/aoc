{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Day18 where

import Linear.V2 (V2)
import Algebra.Graph.Labelled.AdjacencyMap (AdjacencyMap)
import qualified Algebra.Graph.Labelled.AdjacencyMap as AM
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Applicative ((<|>))
import Control.Lens (preview, (^.), (&))
import Control.Lens.Extras (is)
import Control.Lens.TH (makePrisms)
import Data.Char (toLower, isUpper, isLower)
import Util (neighbors2D, Node(..), tile)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Witherable (mapMaybe)
import Data.Maybe (isJust)
import Data.Functor.Extend (extended)

data Tile
  = Empty
  | Wall
  | Door Char
  | Key Char
  | You
  deriving (Show, Eq, Ord)

makePrisms ''Tile

data Delver = Delver
  { _position :: V2 Int
  ,
  }

data S = S
  { _delvers :: [Delver]
  , _keys    :: Map (V2 Int) Char
  , _doors   :: Map (V2 Int) Char
  , _walls   :: Map (V2 Int) Char
  ,
  }

data F = F
  { _es :: Map Char (Map Char Int)
  }

init :: Map (V2 Int) Tile -> S
init =

unfold :: (S -> ) -> f S
unfold = undefined

type G = Graph Int (Set Char)

neighbors :: Set Char -> G -> Map (Set Char) Int
