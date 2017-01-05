{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Forestay.Data (
    module X
  , foldm
  , id
  , liftMaybe
  , liftEither
  , liftEither'
  , parseDefaultTime
  , tag
)   where

import qualified Protolude.Lifted as Proto
import Protolude.Lifted as X hiding
    ( from
    , fold
    )
import Forestay.BaseNByteString as X

import Data.String as X (fromString, IsString, String)

import Data.Data as X hiding
    ( Fixity, Infix, Prefix
    , gunfold
    )

import Data.DList as X hiding
    ( toList
    , concat
    , map
    , foldr
    , head
    , unfoldr
    , empty
    , list
    , replicate
    , Cons
    , Nil
    , cons
    , snoc
    )

import Data.Vector as X
    ( Vector
    )

import Data.Time as X
import Data.Hashable.Time as X

import Safe as X hiding
    ( at
    )

id :: a -> a
id = identity
{-# INLINE id #-}

{-|
  A good way to enter times into data structures.

  Combine with 'runIdentity' for static structures.
-}
parseDefaultTime :: (Monad m, ParseTime t) => String -> m t
parseDefaultTime = parseTimeM True defaultTimeLocale "%0Y-%m-%d %H:%M:%S %Z"
{-# INLINE parseDefaultTime #-}

foldm :: (Foldable t, Monoid m) => t m -> m
foldm = Proto.fold
{-# INLINE foldm #-}

liftMaybe :: Alternative f => Maybe a -> f a
liftMaybe = maybe empty pure
{-# INLINE liftMaybe #-}

liftEither :: MonadError e m => Either e a -> m a
liftEither = throwError `either` pure
{-# INLINE liftEither #-}

liftEither' :: Alternative f => Either b a -> f a
liftEither' = const empty `either` pure
{-# INLINE liftEither' #-}

{-|
  Convert a Proxy based representation to a TypeApplication based representation.

  /e.g./ instead of

  > throw (Proxy :: Proxy tag)

  or

  > throw $ Proxy @tag

  write

  > tag @tag throw
-}
tag :: forall tag t . (Proxy tag -> t) -> t
tag = ($ Proxy @tag)
{-# INLINE tag #-}

