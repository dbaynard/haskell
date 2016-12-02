{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Forestay.Pipes (
    module Forestay.Pipes
  , module X
)   where

import Protolude.Lifted
import Forestay.Lens

import qualified Pipes
import Pipes as X hiding
    ( Proxy
    , (<~)
    , for
    , yield
    , each
    , next
    , every
    , discard
    )

import qualified Pipes.Prelude as Pipes
import Pipes.Prelude as X hiding
    ( all
    , and
    , any
    , concat
    , drop
    , dropWhile
    , elem
    , filter
    , filterM
    , find
    , fold
    , foldM
    , head
    , index
    , length
    , map
    , mapM
    , mapM_
    , maximum
    , minimum
    , notElem
    , null
    , or
    , print
    , product
    , replicateM
    , seq
    , sequence
    , show
    , sum
    , take
    , takeWhile
    , toList
    , unfoldr
    , zip
    , zipWith
    )

import qualified Pipes.Parse as Pipes
import Pipes.Parse as X hiding
    ( StateT, runStateT, evalStateT, execStateT
    , yield
    , group
    , splitAt
    , span
    , groupBy
    , skip, draw, skipAll, drawAll, unDraw, peek, isEndOfInput
    )

import qualified Pipes.Text as PT
import Pipes.Text as X hiding
    ( all
    , any
    , break
    , chunksOf
    , concatMap
    , drop
    , dropWhile
    , filter
    , find
    , fromLazy
    , group
    , groupBy
    , groups
    , groupsBy
    , head
    , index
    , intercalate
    , intersperse
    , last
    , length
    , line
    , lines
    , map
    , maximum
    , minimum
    , null
    , pack
    , scan
    , span
    , splitAt
    , splits
    , splitsWith
    , take
    , takeWhile
    , toLazy
    , toLazyM
    , unlines
    , unpack
    , unwords
    , word
    , words
    )

import qualified Pipes.ByteString as PB
import Pipes.ByteString as X hiding
    ( all
    , any
    , break
    , chunksOf
    , concatMap
    , drop
    , dropWhile
    , elem
    , elemIndices
    , filter
    , find
    , findIndex
    , findIndices
    , fromHandle
    , fromLazy
    , group
    , groupBy
    , groups
    , groupsBy
    , head
    , index
    , intersperse
    , last
    , length
    , line
    , lines
    , map
    , maximum
    , minimum
    , notElem
    , null
    , pack
    , scan
    , span
    , splitAt
    , splits
    , splitsWith
    , stdin
    , stdout
    , take
    , takeWhile
    , toHandle
    , toLazy
    , toLazyM
    , unlines
    , unpack
    , unwords
    , word
    , words
    )

type PProxy = Pipes.Proxy

(<~~) :: Monad m
    => (b -> PProxy x' x c' c m b')
    -> (a -> PProxy x' x b' b m a')
    -> a -> PProxy x' x c' c m a'
(<~~) = (Pipes.<~)
{-# INLINE (<~~) #-}

forP :: Monad m
    => PProxy x' x c' c m a'
    -> (c -> PProxy x' x b' b m c')
    -> PProxy x' x b' b m a'
forP = Pipes.for
{-# INLINE forP #-}

yieldP :: Monad m => a -> Producer' a m ()
yieldP = Pipes.yield
{-# INLINE yieldP #-}

nextP :: Monad m => Producer a m r -> m (Either r (a, Producer a m r))
nextP = Pipes.next
{-# INLINE nextP #-}

eachP :: (Monad m, Foldable f) => f a -> Producer' a m ()
eachP = Pipes.each
{-# INLINE eachP #-}

everyP :: (Monad m, Enumerable t) => t m a -> Producer' a m ()
everyP = Pipes.every
{-# INLINE everyP #-}

discardP :: Monad m => a -> m ()
discardP = Pipes.discard
{-# INLINE discardP #-}

unfoldP :: Monad m => (s -> m (Either r (a, s))) -> s -> Producer a m r
unfoldP = Pipes.unfoldr
{-# INLINE unfoldP #-}

spanP :: Monad m => (a -> Bool) -> Producer a m x :~> Producer a m (Producer a m x)
spanP = Pipes.span
{-# INLINE spanP #-}

splitAtP :: Monad m => Int -> Producer a m x :~> Producer a m (Producer a m x)
splitAtP = Pipes.splitAt
{-# INLINE splitAtP #-}

groupByP :: Monad m => (a -> a -> Bool) -> Producer a m x :~> Producer a m (Producer a m x)
groupByP = Pipes.groupBy
{-# INLINE groupByP #-}

groupP :: (Monad m, Eq a) => Producer a m x :~> Producer a m (Producer a m x)
groupP = Pipes.group
{-# INLINE groupP #-}

drawP :: Monad m => Parser a m (Maybe a)
drawP = Pipes.draw
{-# INLINE drawP #-}

skipP :: Monad m => Parser a m Bool
skipP = Pipes.skip
{-# INLINE skipP #-}

drawAllP :: Monad m => Parser a m [a]
drawAllP = Pipes.drawAll
{-# INLINE drawAllP #-}

skipAllP :: Monad m => Parser a m ()
skipAllP = Pipes.skipAll
{-# INLINE skipAllP #-}

unDrawP :: Monad m => a -> Parser a m ()
unDrawP = Pipes.unDraw
{-# INLINE unDrawP #-}

peekP :: Monad m => Parser a m (Maybe a)
peekP = Pipes.peek
{-# INLINE peekP #-}

isEndOfInputP :: Monad m => Parser a m Bool
isEndOfInputP = Pipes.isEndOfInput
{-# INLINE isEndOfInputP #-}

--------------------------------------------------
-- * ByteString
--------------------------------------------------

stdinB :: MonadIO m => Producer' ByteString m ()
stdinB = PB.stdin
{-# INLINE stdinB #-}

stdoutB :: MonadIO m => Consumer' ByteString m ()
stdoutB = PB.stdout
{-# INLINE stdoutB #-}

