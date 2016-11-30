{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}

module Forestay.ListT (
    module Forestay.ListT
  , module X
)   where

import Protolude.Lifted

import List.Transformer as X hiding
    ( next
    , unfold
    , drop
    , take
    , zip
    , fold
    , foldM
    )
import qualified List.Transformer as LT

pattern ConsT :: a -> ListT m a -> Step m a
pattern ConsT a x = LT.Cons a x

pattern NilT :: Step m a
pattern NilT = LT.Nil

nextT :: ListT m a -> m (Step m a)
nextT = LT.next
{-# INLINE nextT #-}

unfoldT :: Monad m => (b -> m (Maybe (a, b))) -> b -> ListT m a
unfoldT = LT.unfold
{-# INLINE unfoldT #-}

dropT :: Monad m => Int -> ListT m a -> ListT m a
dropT = LT.drop
{-# INLINE dropT #-}

takeT :: Monad m => Int -> ListT m a -> ListT m a
takeT = LT.take
{-# INLINE takeT #-}

zipT :: Monad m => ListT m a -> ListT m b -> ListT m (a, b)
zipT = LT.zip
{-# INLINE zipT #-}

foldT :: Monad m => (x -> a -> x) -> x -> (x -> b) -> ListT m a -> m b
foldT = LT.fold
{-# INLINE foldT #-}

foldMT :: Monad m => (x -> a -> m x) -> m x -> (x -> m b) -> ListT m a -> m b
foldMT = LT.foldM
{-# INLINE foldMT #-}

