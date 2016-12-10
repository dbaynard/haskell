{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}

module Forestay.Data (
    module X
  , id
  , parseDefaultTime
  , foldm
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

