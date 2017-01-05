{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}

module Forestay (
    module X
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
    , liftEither, liftEither' -- reimplement here to use ether
    , getLine, readFile, writeFile -- chunked-data
    )
import Forestay.ByteString as X
import Forestay.Error as X
import Forestay.Ether as X
import Forestay.Lens as X
import Forestay.Pipes as X

import Data.Tagged as X

import Data.Functor.Foldable as X

import Data.IOData as X

import ReadArgs as X hiding
    ( parse
    , name
    )
import qualified ReadArgs
    ( parse
    , name
    )

import Data.Monoid.Instances.ByteString.UTF8 as X

--------------------------------------------------
-- * ReadArgs
--------------------------------------------------

parseAnArg :: Arguable a => String -> Maybe a
parseAnArg = ReadArgs.parse
{-# INLINE parseAnArg #-}

nameAnArg :: Arguable a => a -> String
nameAnArg = ReadArgs.name
{-# INLINE nameAnArg #-}
