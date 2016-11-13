{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}

module Forestay.Lens (
    module Forestay.Lens
  , module X
)   where

import Protolude.Lifted (State, execState)
import Control.Lens as X
import Data.Text.Lens as X

type s :~> a = Lens s s a a
type s :~>> a = Traversal s s a a
type s :~>: a = Prism s s a a
type s :~~: a = Iso s s a a

type Assign a = State a ()

def :: Assign a
def = pure ()

makeAssign :: a -> Assign a -> a
makeAssign = flip execState
{-# INLINE makeAssign #-}

(<-$) :: a -> Assign a -> a
(<-$) = makeAssign
infixl 2 <-$
{-# INLINE (<-$) #-}
