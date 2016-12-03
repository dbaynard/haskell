{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}

module Forestay.Data (
    module X
  , id
  , parseDefaultTime
)   where

import Protolude.Lifted as X hiding
    ( from
    )
import Forestay.BaseNByteString as X

import Data.String as X (fromString, IsString, String)

import Data.Data as X hiding (Fixity, Infix, Prefix)

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
