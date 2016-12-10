{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeOperators, RankNTypes #-}

module Forestay.Containers (
    module X
  , hashSetOf
  , intSetOf
  , hashSetmapped
  , intSetmapped
  , intMembers
  , vectorSliced
)   where

import Forestay.Lens
import Forestay.Data

import Data.Array.Lens as X
import Data.List.Lens as X
import Data.Sequence.Lens as X
import Data.Set.Lens as X
import Data.Tree.Lens as X
import Data.Vector.Lens as X hiding
    ( sliced
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
    , members
    )
import qualified Data.Vector.Lens as Vector
    ( sliced
    )

hashSetOf :: Hashable a => Getting (HashSet a) s a -> s -> HashSet a
hashSetOf = HashSet.setOf
{-# INLINE hashSetOf #-}

intSetOf :: Getting IntSet s Int -> s -> IntSet
intSetOf = IntSet.setOf
{-# INLINE intSetOf #-}

hashSetmapped :: (Eq j, Hashable j) => IndexPreservingSetter (HashSet i) (HashSet j) i j
hashSetmapped = HashSet.setmapped
{-# INLINE hashSetmapped #-}

intSetmapped :: IndexPreservingSetter' IntSet Int
intSetmapped = IntSet.setmapped
{-# INLINE intSetmapped #-}

intMembers :: Fold IntSet Int
intMembers = IntSet.members
{-# INLINE intMembers #-}

vectorSliced :: Int -> Int -> Vector a :~> Vector a
vectorSliced = Vector.sliced
{-# INLINE vectorSliced #-}

