{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}

module Forestay.Lens (
    module Forestay.Lens
  , module X
)   where

import Protolude.Lifted (State, execState)
import Control.Lens as X
import Data.Text.Lens as X
import Data.ByteString.Lens as X
import Data.Data.Lens as X
import Numeric.Lens as X
import System.FilePath.Lens as X
import Data.Time.Lens as X hiding
    ( years
    , months
    , days
    , hours
    , minutes
    , seconds
    )

import Data.Fixed (Pico)
import qualified Data.Time.Lens as Modified
    ( years
    , months
    , days
    , hours
    , minutes
    , seconds
    )

type s :~> a = Lens s s a a
type s :~>> a = Traversal s s a a
type s :~>: a = Prism s s a a
type s :~~: a = Iso s s a a

type Assign a = State a ()

def :: Assign a
def = pure ()
{-# INLINE def #-}

makeAssign :: a -> Assign a -> a
makeAssign = flip execState
{-# INLINE makeAssign #-}

(<-$) :: a -> Assign a -> a
(<-$) = makeAssign
infixl 2 <-$
{-# INLINE (<-$) #-}

fview :: Functor f => Getting a s a -> f s -> f a
fview = fmap . view
{-# INLINE fview #-}

years' :: Dateable d => d :~> Integer
years' = Modified.years
{-# INLINE years' #-}

months' :: Dateable d => d :~> Int
months' = Modified.months
{-# INLINE months' #-}

days' :: Dateable d => d :~> Int
days' = Modified.days
{-# INLINE days' #-}

hours' :: Timeable t => t :~> Int
hours' = Modified.hours
{-# INLINE hours' #-}

minutes' :: Timeable t => t :~> Int
minutes' = Modified.minutes
{-# INLINE minutes' #-}

seconds' :: Timeable t => t :~> Pico
seconds' = Modified.seconds
{-# INLINE seconds' #-}
