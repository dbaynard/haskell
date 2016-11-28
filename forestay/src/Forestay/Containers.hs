{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeOperators, RankNTypes #-}

module Forestay.Containers (
    module Forestay.Containers
  , module X
)   where

import Forestay.Lens

import Data.Array.Lens as X
import Data.IntSet.Lens as X hiding
    ( setOf
    , setmapped
    )
import Data.List.Lens as X
import Data.Sequence.Lens as X
import Data.Set.Lens as X
import Data.Tree.Lens as X
import Data.Vector.Lens as X hiding
    ( sliced
    )

import Data.Vector as X
    ( Vector
    )
import Data.HashSet as X
    ( HashSet
    )
import Data.IntSet as X
    ( IntSet
    )
import Data.Hashable as X
    ( Hashable
    )

import qualified Data.HashSet.Lens as HashSet
    ( setOf
    , setmapped
    )
import qualified Data.IntSet.Lens as IntSet
    ( setOf
    , setmapped
    )
import qualified Data.Vector.Lens as Vector
    ( sliced
    )

hashSetOf :: Hashable a => Getting (HashSet a) s a -> s -> HashSet a
hashSetOf = HashSet.setOf

intSetOf :: Getting IntSet s Int -> s -> IntSet
intSetOf = IntSet.setOf

hashSetmapped :: (Eq j, Hashable j) => IndexPreservingSetter (HashSet i) (HashSet j) i j
hashSetmapped = HashSet.setmapped

intSetmapped :: IndexPreservingSetter' IntSet Int
intSetmapped = IntSet.setmapped

vectorSliced :: Int -> Int -> Vector a :~> Vector a
vectorSliced = Vector.sliced
