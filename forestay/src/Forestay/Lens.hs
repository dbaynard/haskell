{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}

module Forestay.Lens (
    module Forestay.Lens
  , module X
)   where

import Forestay.Data
    (  Proxy(Proxy)
    )
import qualified Control.Lens as L
import Control.Lens as X hiding
    ( (.=)
    , (%=)
    , (?=)
    , (<>=)
    , (<~)
    , (??)
    , para
    , noneOf
    )
import Data.Text.Lens as X
import Data.ByteString.Lens as X
import Data.Data.Lens as X
import qualified Numeric.Lens as L
import Numeric.Lens as X hiding
    ( binary
    , octal
    , decimal
    , hex
    )
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

import qualified Control.Monad.Ether.Implicit as Ether
import Data.Monoid (Any)

type s :~> a = Lens s s a a
type s :~>> a = Traversal s s a a
type s :~>: a = Prism s s a a
type s :~~: a = Iso s s a a

type Assign a = Ether.State a ()

data Settings
settings :: Proxy Settings
settings = Proxy

(.=) :: Ether.MonadState s m => ASetter s s a b -> b -> m ()
l .= b = Ether.modify (l .~ b)
infix 4 .=
{-# INLINE (.=) #-}

(<~) :: Ether.MonadState s m => ASetter s s a b -> m b -> m ()
l <~ mb = mb >>= (l .=)
infixr 2 <~
{-# INLINE (<~) #-}

(%=) :: Ether.MonadState s m => ASetter s s a b -> (a -> b) -> m ()
l %= f = Ether.modify (l %~ f)
infix 4 %=
{-# INLINE (%=) #-}

(?=) :: Ether.MonadState s m => ASetter s s a (Maybe b) -> b -> m ()
l ?= b = Ether.modify (l ?~ b)
infix 4 ?=
{-# INLINE (?=) #-}

(<>=) :: (Ether.MonadState s m, Monoid a) => ASetter' s a -> a -> m ()
l <>= a = Ether.modify (l <>~ a)
infix 4 <>=
{-# INLINE (<>=) #-}

def :: Assign a
def = pure ()
{-# INLINE def #-}

makeAssign :: a -> Assign a -> a
makeAssign = flip Ether.execState
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

noneOf' :: Getting Any s a -> (a -> Bool) -> s -> Bool
noneOf' = L.noneOf
{-# INLINE noneOf' #-}

binary' :: Integral a => Prism' String a
binary' = L.binary
{-# INLINE binary' #-}

octal' :: Integral a => Prism' String a
octal' = L.octal
{-# INLINE octal' #-}

decimal' :: Integral a => Prism' String a
decimal' = L.decimal
{-# INLINE decimal' #-}

hex' :: Integral a => Prism' String a
hex' = L.hex
{-# INLINE hex' #-}

