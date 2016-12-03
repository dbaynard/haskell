{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}

module Forestay (
    module X
  , ether
  , parseAnArg
  , nameAnArg
)   where

import Forestay.Data as X hiding
    ( Leniency(..)
    , uncons
    , unsnoc
    , to
    , (<.>)
    , (&)
    , (<$!>)
    , get, gets, put, local, reader, ask, asks, modify, state
    , catch, handle
    , StateT, ReaderT, ExceptT, State, Reader, Except
    , MonadState, MonadReader
    , runReader, runReaderT
    , evalState, evalStateT, runState, runStateT, execState, execStateT
    , runExcept, runExceptT
    , getLine, readFile, writeFile -- chunked-data
    )
import Forestay.Error as X
import Forestay.Lens as X
import Forestay.Pipes as X

import Control.Monad.Ether as X
import Control.Ether.Abbr as X
import Data.Tagged as X

import Data.Functor.Foldable as X

import Data.Coerce (coerce)

import Data.IOData as X

import ReadArgs as X hiding
    ( parse
    , name
    )
import qualified ReadArgs
    ( parse
    , name
    )

ether :: Coercible a (DispatchT ('TagAttach tag) m b) => proxy tag -> a -> m b
ether tag = tagAttach tag . coerce
{-# INLINE ether #-}

--------------------------------------------------
-- * ReadArgs
--------------------------------------------------

parseAnArg :: Arguable a => String -> Maybe a
parseAnArg = ReadArgs.parse
{-# INLINE parseAnArg #-}

nameAnArg :: Arguable a => a -> String
nameAnArg = ReadArgs.name
{-# INLINE nameAnArg #-}
