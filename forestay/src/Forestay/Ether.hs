{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Forestay.Ether (
    module X
  , StateS
  , StateST
  , ether
  , ether'
  , liftEither
  , liftEither'
  , runStateS
  , evalStateS
  , execStateS
  , stateST
  , runStateST
  , evalStateST
  , execStateST
)   where

import Forestay.Data
    ( Proxy(Proxy)
    , Coercible
    , tag
    )

import qualified Control.Monad.Ether.State.Strict as S
import Control.Monad.Ether as X
import Control.Ether.Abbr as X

import Data.Coerce (coerce)

--------------------------------------------------
-- * Ether
--------------------------------------------------

ether :: forall tag a m b .
           Coercible a (DispatchT ('TagAttach tag) m b)
        => a -> m b
ether = tagAttach @_ @tag Proxy . coerce
{-# INLINE ether #-}

ether' :: Coercible a (DispatchT ('TagAttach tag) m b)
        => proxy tag -> a -> m b
ether' prx = tagAttach prx . coerce
{-# INLINE ether' #-}

--------------------------------------------------
-- * MonadExcept
--------------------------------------------------

liftEither
        :: forall tag a e m .
           MonadExcept tag e m
        => Either e a -> m a
liftEither = tag @tag throw `either` pure
{-# INLINE liftEither #-}

liftEither' :: MonadExcept tag e m => proxy tag -> Either e a -> m a
liftEither' prx = throw prx `either` pure
{-# INLINE liftEither' #-}

--------------------------------------------------
-- * Strict state
--------------------------------------------------

type StateS tag r = S.State tag r

runStateS :: proxy tag -> StateS tag s a -> s -> (a, s)
runStateS = S.runState
{-# INLINE runStateS #-}

evalStateS :: proxy tag -> StateS tag s a -> s -> a
evalStateS = S.evalState
{-# INLINE evalStateS #-}

execStateS :: proxy tag -> StateS tag s a -> s -> s
execStateS = S.execState
{-# INLINE execStateS #-}

type StateST tag s = S.StateT tag s

stateST :: proxy tag -> (s -> m (a, s)) -> StateST tag s m a
stateST = S.stateT
{-# INLINE stateST #-}

runStateST :: proxy tag -> StateST tag s m a -> s -> m (a, s)
runStateST = S.runStateT
{-# INLINE runStateST #-}

evalStateST :: Monad m => proxy tag -> StateST tag s m a -> s -> m a
evalStateST = S.evalStateT
{-# INLINE evalStateST #-}

execStateST :: Monad m => proxy tag -> StateST tag s m a -> s -> m s
execStateST = S.execStateT
{-# INLINE execStateST #-}

