{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Forestay.Pipes (
    module Forestay.Pipes
  , module X
)   where

import Forestay.Data
import Forestay.Lens

import Data.IOData

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
    , embed
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
    , read
    , readLn
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
    -- not quite clashing
    , elemIndices
    , findIndex
    , findIndices
    , fold'
    , foldM'
    , last
    , repeatM
    , scan
    , scanM
    , takeWhile
    , toListM
    , toListM'
    , fromHandle
    , toHandle
    , stdinLn -- :: MonadIO m => Producer' String m ()
    , stdoutLn -- :: MonadIO m => Consumer' String m ()
    , stdoutLn' -- :: MonadIO m => Consumer' String m r
    )

import Pipes.Lift as X
    ( distribute
    , liftCatchError
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
    -- Not quite conflicting
    , toCaseFold
    , toLower
    , toUpper
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
    , toLazy
    , toLazyM
    , unlines
    , unpack
    , unwords
    , word
    , words
    -- Not 'clashing', but anyway
    , chunksOf'
    , count
    , elemIndex
    )

--------------------------------------------------
-- * Pipes
--------------------------------------------------

type PProxy = Pipes.Proxy
type PipesStateT = Pipes.StateT
type Producer2 a b m r = Producer b m (Producer a m r)
type Parser2 a b m r = forall x . Pipes.StateT (Producer a m x) (Producer b m) r

embedM :: (MMonad t, Monad n)
       => (forall a . m a -> t n a)
       -> t m b -> t n b
embedM = Pipes.embed
{-# INLINE embedM #-}

(<~~) :: Monad m
    => (b -> PProxy x' x c' c m b')
    -> (a -> PProxy x' x b' b m a')
    -> a -> PProxy x' x c' c m a'
(<~~) = (Pipes.<~)
infixl 4 <~~
{-# INLINE (<~~) #-}

(>-&>) :: Producer a m x -> Parser a m r -> m (r, Producer a m x)
(>-&>) = flip Pipes.runStateT
infixl 8 >-&>
{-# INLINE (>-&>) #-}

(<&-<) :: Parser a m r -> Producer a m x -> m (r, Producer a m x)
(<&-<) = Pipes.runStateT
infixl 8 <&-<
{-# INLINE (<&-<) #-}

(>-|>) :: Monad m => Producer a m x -> Parser a m r -> m (Producer a m x)
(>-|>) = flip Pipes.execStateT
infixl 8 >-|>
{-# INLINE (>-|>) #-}

(<|-<) :: Monad m => Parser a m r -> Producer a m x -> m (Producer a m x)
(<|-<) = Pipes.execStateT
infixl 8 <|-<
{-# INLINE (<|-<) #-}

(>->>) :: Monad m => Producer a m x -> Parser a m r -> m r
(>->>) = flip Pipes.evalStateT
infixl 8 >->>
{-# INLINE (>->>) #-}

(<<-<) :: Monad m => Parser a m r -> Producer a m x -> m r
(<<-<) = Pipes.evalStateT
infixl 8 <<-<
{-# INLINE (<<-<) #-}

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
-- * Prelude
--------------------------------------------------

allP :: Monad m => (a -> Bool) -> Producer a m () -> m Bool
allP = Pipes.all
{-# INLINE allP #-}

andP :: Monad m => Producer Bool m () -> m Bool
andP = Pipes.and
{-# INLINE andP #-}

anyP :: Monad m => (a -> Bool) -> Producer a m () -> m Bool
anyP = Pipes.any
{-# INLINE anyP #-}

concatP :: (Monad m, Foldable f) => Pipe (f a) a m r
concatP = Pipes.concat
{-# INLINE concatP #-}

dropP :: Monad m => Int -> Pipe a a m r
dropP = Pipes.drop
{-# INLINE dropP #-}

dropWhileP :: Monad m => (a -> Bool) -> Pipe a a m r
dropWhileP = Pipes.dropWhile
{-# INLINE dropWhileP #-}

elemP :: (Monad m, Eq a) => a -> Producer a m () -> m Bool
elemP = Pipes.elem
{-# INLINE elemP #-}

elemIndicesP :: (Monad m, Eq a) => a -> Pipe a Int m r
elemIndicesP = Pipes.elemIndices
{-# INLINE elemIndicesP #-}

filterP :: Monad m => (a -> Bool) -> Pipe a a m r
filterP = Pipes.filter
{-# INLINE filterP #-}

filterMP :: Monad m => (a -> m Bool) -> Pipe a a m r
filterMP = Pipes.filterM
{-# INLINE filterMP #-}

findP :: Monad m => (a -> Bool) -> Producer a m () -> m (Maybe a)
findP = Pipes.find
{-# INLINE findP #-}

findIndexP :: Monad m => (a -> Bool) -> Producer a m () -> m (Maybe Int)
findIndexP = Pipes.findIndex
{-# INLINE findIndexP #-}

findIndicesP :: Monad m => (a -> Bool) -> Pipe a Int m r
findIndicesP = Pipes.findIndices
{-# INLINE findIndicesP #-}

foldP :: Monad m => (x -> a -> x) -> x -> (x -> b) -> Producer a m () -> m b
foldP = Pipes.fold
{-# INLINE foldP #-}

fold'P :: Monad m => (x -> a -> x) -> x -> (x -> b) -> Producer a m r -> m (b, r)
fold'P = Pipes.fold'
{-# INLINE fold'P #-}

foldMP :: Monad m => (x -> a -> m x) -> m x -> (x -> m b) -> Producer a m () -> m b
foldMP = Pipes.foldM
{-# INLINE foldMP #-}

foldM'P :: Monad m => (x -> a -> m x) -> m x -> (x -> m b) -> Producer a m r -> m (b, r)
foldM'P = Pipes.foldM'
{-# INLINE foldM'P #-}

headP :: Monad m => Producer a m () -> m (Maybe a)
headP = Pipes.head
{-# INLINE headP #-}

indexP :: Monad m => Int -> Producer a m () -> m (Maybe a)
indexP = Pipes.index
{-# INLINE indexP #-}

lastP :: Monad m => Producer a m () -> m (Maybe a)
lastP = Pipes.last
{-# INLINE lastP #-}

lengthP :: Monad m => Producer a m () -> m Int
lengthP = Pipes.length
{-# INLINE lengthP #-}

mapP :: Monad m => (a -> b) -> Pipe a b m r
mapP = Pipes.map
{-# INLINE mapP #-}

mapMP :: Monad m => (a -> m b) -> Pipe a b m r
mapMP = Pipes.mapM
{-# INLINE mapMP #-}

mapM_P :: Monad m => (a -> m ()) -> Consumer' a m r
mapM_P = Pipes.mapM_
{-# INLINE mapM_P #-}

maximumP :: (Monad m, Ord a) => Producer a m () -> m (Maybe a)
maximumP = Pipes.maximum
{-# INLINE maximumP #-}

minimumP :: (Monad m, Ord a) => Producer a m () -> m (Maybe a)
minimumP = Pipes.minimum
{-# INLINE minimumP #-}

notElemP :: (Monad m, Eq a) => a -> Producer a m () -> m Bool
notElemP = Pipes.notElem
{-# INLINE notElemP #-}

nullP :: Monad m => Producer a m () -> m Bool
nullP = Pipes.null
{-# INLINE nullP #-}

orP :: Monad m => Producer Bool m () -> m Bool
orP = Pipes.or
{-# INLINE orP #-}

printP :: (MonadIO m, Show a) => Consumer' a m r
printP = Pipes.print
{-# INLINE printP #-}

productP :: (Monad m, Num a) => Producer a m () -> m a
productP = Pipes.product
{-# INLINE productP #-}

readP :: (Monad m, Read a) => Pipe String a m r
readP = Pipes.read
{-# INLINE readP #-}

readLnP :: (MonadIO m, Read a) => Producer' a m ()
readLnP = Pipes.readLn
{-# INLINE readLnP #-}

repeatMP :: Monad m => m a -> Producer' a m r
repeatMP = Pipes.repeatM
{-# INLINE repeatMP #-}

replicateMP :: Monad m => Int -> m a -> Producer' a m ()
replicateMP = Pipes.replicateM
{-# INLINE replicateMP #-}

scanP :: Monad m => (x -> a -> x) -> x -> (x -> b) -> Pipe a b m r
scanP = Pipes.scan
{-# INLINE scanP #-}

scanMP :: Monad m => (x -> a -> m x) -> m x -> (x -> m b) -> Pipe a b m r
scanMP = Pipes.scanM
{-# INLINE scanMP #-}

seqP :: Monad m => Pipe a a m r
seqP = Pipes.seq
{-# INLINE seqP #-}

sequenceP :: Monad m => Pipe (m a) a m r
sequenceP = Pipes.sequence
{-# INLINE sequenceP #-}

sumP :: (Monad m, Num a) => Producer a m () -> m a
sumP = Pipes.sum
{-# INLINE sumP #-}

takeP :: Monad m => Int -> Pipe a a m ()
takeP = Pipes.take
{-# INLINE takeP #-}

takeWhileP :: Monad m => (a -> Bool) -> Pipe a a m ()
takeWhileP = Pipes.takeWhile
{-# INLINE takeWhileP #-}

takeWhile'P :: Monad m => (a -> Bool) -> Pipe a a m a
takeWhile'P = Pipes.takeWhile'
{-# INLINE takeWhile'P #-}

toListP :: Producer a Identity () -> [a]
toListP = Pipes.toList
{-# INLINE toListP #-}

toListMP :: Monad m => Producer a m () -> m [a]
toListMP = Pipes.toListM
{-# INLINE toListMP #-}

toListM'P :: Monad m => Producer a m r -> m ([a], r)
toListM'P = Pipes.toListM'
{-# INLINE toListM'P #-}

zipP :: Monad m => Producer a m r -> Producer b m r -> Producer' (a, b) m r
zipP = Pipes.zip
{-# INLINE zipP #-}

--------------------------------------------------
-- * ByteString
--------------------------------------------------

stdinB :: MonadIO m => Producer' ByteString m ()
stdinB = PB.stdin
{-# INLINE stdinB #-}

stdoutB :: MonadIO m => Consumer' ByteString m ()
stdoutB = PB.stdout
{-# INLINE stdoutB #-}

allB :: Monad m => (Word8 -> Bool) -> Producer ByteString m () -> m Bool
allB = PB.all
{-# INLINE allB #-}

anyB :: Monad m => (Word8 -> Bool) -> Producer ByteString m () -> m Bool
anyB = PB.any
{-# INLINE anyB #-}

breakB :: Monad m => (Word8 -> Bool) -> Producer ByteString m x :~> Producer ByteString m (Producer ByteString m x)
breakB = PB.break
{-# INLINE breakB #-}

chunksOfB :: (Monad m, Integral n) => n -> Producer ByteString m x :~> FreeT (Producer ByteString m) m x
chunksOfB = PB.chunksOf
{-# INLINE chunksOfB #-}

chunksOfB' :: (Monad m, Integral n) => n -> Producer ByteString m r -> Producer ByteString m r
chunksOfB' = PB.chunksOf'
{-# INLINE chunksOfB' #-}

concatMapB :: Monad m => (Word8 -> ByteString) -> Pipe ByteString ByteString m r
concatMapB = PB.concatMap
{-# INLINE concatMapB #-}

countB :: (Monad m, Num n) => Word8 -> Producer ByteString m () -> m n
countB = PB.count
{-# INLINE countB #-}

dropB :: (Monad m, Integral n) => n -> Producer ByteString m r -> Producer ByteString m r
dropB = PB.drop
{-# INLINE dropB #-}

dropWhileB :: Monad m => (Word8 -> Bool) -> Producer ByteString m r -> Producer ByteString m r
dropWhileB = PB.dropWhile
{-# INLINE dropWhileB #-}

elemB :: Monad m => Word8 -> Producer ByteString m () -> m Bool
elemB = PB.elem
{-# INLINE elemB #-}

elemIndexB :: (Monad m, Num n) => Word8 -> Producer ByteString m () -> m (Maybe n)
elemIndexB = PB.elemIndex
{-# INLINE elemIndexB #-}

elemIndicesB :: (Monad m, Num n) => Word8 -> Pipe ByteString n m r
elemIndicesB = PB.elemIndices
{-# INLINE elemIndicesB #-}

filterB :: Monad m => (Word8 -> Bool) -> Pipe ByteString ByteString m r
filterB = PB.filter
{-# INLINE filterB #-}

findB :: Monad m => (Word8 -> Bool) -> Producer ByteString m () -> m (Maybe Word8)
findB = PB.find
{-# INLINE findB #-}

findIndexB :: (Monad m, Num n) => (Word8 -> Bool) -> Producer ByteString m () -> m (Maybe n)
findIndexB = PB.findIndex
{-# INLINE findIndexB #-}

findIndicesB :: (Monad m, Num n) => (Word8 -> Bool) -> Pipe ByteString n m r
findIndicesB = PB.findIndices
{-# INLINE findIndicesB #-}

fromHandleB :: MonadIO m => Handle -> Producer' ByteString m ()
fromHandleB = PB.fromHandle
{-# INLINE fromHandleB #-}

fromLazyB :: Monad m => LByteString -> Producer' ByteString m ()
fromLazyB = PB.fromLazy
{-# INLINE fromLazyB #-}

groupB :: Monad m => Producer ByteString m x :~> Producer ByteString m (Producer ByteString m x)
groupB = PB.group
{-# INLINE groupB #-}

groupByB :: Monad m => (Word8 -> Word8 -> Bool) -> Producer ByteString m x :~> Producer ByteString m (Producer ByteString m x)
groupByB = PB.groupBy
{-# INLINE groupByB #-}

groupsB :: Monad m => Producer ByteString m x :~> FreeT (Producer ByteString m) m x
groupsB = PB.groups
{-# INLINE groupsB #-}

groupsByB :: Monad m => (Word8 -> Word8 -> Bool) -> Producer ByteString m x :~> FreeT (Producer ByteString m) m x
groupsByB = PB.groupsBy
{-# INLINE groupsByB #-}

headB :: Monad m => Producer ByteString m () -> m (Maybe Word8)
headB = PB.head
{-# INLINE headB #-}

indexB :: (Monad m, Integral n) => n -> Producer ByteString m () -> m (Maybe Word8)
indexB = PB.index
{-# INLINE indexB #-}

intersperseB :: Monad m => Word8 -> Producer ByteString m r -> Producer ByteString m r
intersperseB = PB.intersperse
{-# INLINE intersperseB #-}

lastB :: Monad m => Producer ByteString m () -> m (Maybe Word8)
lastB = PB.last
{-# INLINE lastB #-}

lengthB :: (Monad m, Num n) => Producer ByteString m () -> m n
lengthB = PB.length
{-# INLINE lengthB #-}

lineB :: Monad m => Producer ByteString m x :~> Producer ByteString m (Producer ByteString m x)
lineB = PB.line
{-# INLINE lineB #-}

linesB :: Monad m => Producer ByteString m x :~> FreeT (Producer ByteString m) m x
linesB = PB.lines
{-# INLINE linesB #-}

mapB :: Monad m => (Word8 -> Word8) -> Pipe ByteString ByteString m r
mapB = PB.map
{-# INLINE mapB #-}

maximumB :: Monad m => Producer ByteString m () -> m (Maybe Word8)
maximumB = PB.maximum
{-# INLINE maximumB #-}

minimumB :: Monad m => Producer ByteString m () -> m (Maybe Word8)
minimumB = PB.minimum
{-# INLINE minimumB #-}

notElemB :: Monad m => Word8 -> Producer ByteString m () -> m Bool
notElemB = PB.notElem
{-# INLINE notElemB #-}

nullB :: Monad m => Producer ByteString m () -> m Bool
nullB = PB.null
{-# INLINE nullB #-}

packB :: Monad m => Producer Word8 m x :~> Producer ByteString m x
packB = PB.pack
{-# INLINE packB #-}

scanB :: Monad m => (Word8 -> Word8 -> Word8) -> Word8 -> Pipe ByteString ByteString m r
scanB = PB.scan
{-# INLINE scanB #-}

spanB :: Monad m => (Word8 -> Bool) -> Producer ByteString m x :~> Producer ByteString m (Producer ByteString m x)
spanB = PB.span
{-# INLINE spanB #-}

splitAtB :: (Monad m, Integral n) => n -> Producer ByteString m x :~> Producer ByteString m (Producer ByteString m x)
splitAtB = PB.splitAt
{-# INLINE splitAtB #-}

splitsB :: Monad m => Word8 -> Producer ByteString m x :~> FreeT (Producer ByteString m) m x
splitsB = PB.splits
{-# INLINE splitsB #-}

splitsWithB :: Monad m => (Word8 -> Bool) -> Producer ByteString m x -> FreeT (Producer ByteString m) m x
splitsWithB = PB.splitsWith
{-# INLINE splitsWithB #-}

takeB :: (Monad m, Integral n) => n -> Pipe ByteString ByteString m ()
takeB = PB.take
{-# INLINE takeB #-}

takeWhileB :: Monad m => (Word8 -> Bool) -> Pipe ByteString ByteString m ()
takeWhileB = PB.takeWhile
{-# INLINE takeWhileB #-}

toHandleB :: MonadIO m => Handle -> Consumer' ByteString m r
toHandleB = PB.toHandle
{-# INLINE toHandleB #-}

toLazyB :: Producer ByteString Identity () -> LByteString
toLazyB = PB.toLazy
{-# INLINE toLazyB #-}

toLazyMB :: Monad m => Producer ByteString m () -> m LByteString
toLazyMB = PB.toLazyM
{-# INLINE toLazyMB #-}

unlinesB :: Monad m => FreeT (Producer ByteString m) m x :~> Producer ByteString m x
unlinesB = PB.unlines
{-# INLINE unlinesB #-}

unpackB :: Monad m => Producer ByteString m x :~> Producer Word8 m x
unpackB = PB.unpack
{-# INLINE unpackB #-}

unwordsB :: Monad m => FreeT (Producer ByteString m) m x -> Producer ByteString m x
unwordsB = PB.unwords
{-# INLINE unwordsB #-}

wordB :: Monad m => Producer ByteString m x :~> Producer ByteString m (Producer ByteString m x)
wordB = PB.word
{-# INLINE wordB #-}

wordsB :: Monad m => Producer ByteString m x -> FreeT (Producer ByteString m) m x
wordsB = PB.words
{-# INLINE wordsB #-}

--------------------------------------------------
-- * Text
--------------------------------------------------

allT :: (Monad m) => (Char -> Bool) -> Producer Text m () -> m Bool
allT = PT.all
{-# INLINE allT #-}

anyT :: (Monad m) => (Char -> Bool) -> Producer Text m () -> m Bool
anyT = PT.any
{-# INLINE anyT #-}

breakT :: (Monad m) => (Char -> Bool) -> Producer Text m r :~> Producer Text m (Producer Text m r)
breakT = PT.break
{-# INLINE breakT #-}

chunksOfT :: (Monad m, Integral n) => n -> Producer Text m r :~> FreeT (Producer Text m) m r
chunksOfT = PT.chunksOf
{-# INLINE chunksOfT #-}

concatMapT :: (Monad m) => (Char -> Text) -> Pipe Text Text m r
concatMapT = PT.concatMap
{-# INLINE concatMapT #-}

dropT :: (Monad m, Integral n) => n -> Producer Text m r -> Producer Text m r
dropT = PT.drop
{-# INLINE dropT #-}

dropWhileT :: (Monad m) => (Char -> Bool) -> Producer Text m r -> Producer Text m r
dropWhileT = PT.dropWhile
{-# INLINE dropWhileT #-}

filterT :: (Monad m) => (Char -> Bool) -> Pipe Text Text m r
filterT = PT.filter
{-# INLINE filterT #-}

findT :: (Monad m) => (Char -> Bool) -> Producer Text m () -> m (Maybe Char)
findT = PT.find
{-# INLINE findT #-}

fromLazyT :: (Monad m) => LText -> Producer' Text m ()
fromLazyT = PT.fromLazy
{-# INLINE fromLazyT #-}

groupT :: Monad m => Producer Text m r :~> Producer Text m (Producer Text m r)
groupT = PT.group
{-# INLINE groupT #-}

groupByT :: (Monad m) => (Char -> Char -> Bool) -> Producer Text m r :~> Producer Text m (Producer Text m r)
groupByT = PT.groupBy
{-# INLINE groupByT #-}

groupsT :: Monad m => Producer Text m x :~> FreeT (Producer Text m) m x
groupsT = PT.groups
{-# INLINE groupsT #-}

groupsByT :: Monad m => (Char -> Char -> Bool) -> Producer Text m x :~> FreeT (Producer Text m) m x
groupsByT = PT.groupsBy
{-# INLINE groupsByT #-}

headT :: (Monad m) => Producer Text m () -> m (Maybe Char)
headT = PT.head
{-# INLINE headT #-}

indexT :: (Monad m, Integral a) => a-> Producer Text m () -> m (Maybe Char)
indexT = PT.index
{-# INLINE indexT #-}

intercalateT :: (Monad m) => Producer Text m () -> FreeT (Producer Text m) m r -> Producer Text m r
intercalateT = PT.intercalate
{-# INLINE intercalateT #-}

intersperseT :: (Monad m) => Char -> Producer Text m r -> Producer Text m r
intersperseT = PT.intersperse
{-# INLINE intersperseT #-}

lastT :: (Monad m) => Producer Text m () -> m (Maybe Char)
lastT = PT.last
{-# INLINE lastT #-}

lengthT :: (Monad m, Num n) => Producer Text m () -> m n
lengthT = PT.length
{-# INLINE lengthT #-}

lineT :: (Monad m) => Producer Text m r :~> Producer Text m (Producer Text m r)
lineT = PT.line
{-# INLINE lineT #-}

linesT :: (Monad m) => Producer Text m r :~>  FreeT (Producer Text m) m r
linesT = PT.lines
{-# INLINE linesT #-}

mapT :: (Monad m) => (Char -> Char) -> Pipe Text Text m r
mapT = PT.map
{-# INLINE mapT #-}

maximumT :: (Monad m) => Producer Text m () -> m (Maybe Char)
maximumT = PT.maximum
{-# INLINE maximumT #-}

minimumT :: (Monad m) => Producer Text m () -> m (Maybe Char)
minimumT = PT.minimum
{-# INLINE minimumT #-}

nullT :: (Monad m) => Producer Text m () -> m Bool
nullT = PT.null
{-# INLINE nullT #-}

packT :: Monad m => Producer Char m r :~> Producer Text m r
packT = PT.pack
{-# INLINE packT #-}

scanT :: (Monad m) => (Char -> Char -> Char) -> Char -> Pipe Text Text m r
scanT = PT.scan
{-# INLINE scanT #-}

spanT :: (Monad m) => (Char -> Bool) -> Producer Text m r :~> Producer Text m (Producer Text m r)
spanT = PT.span
{-# INLINE spanT #-}

splitAtT :: (Monad m, Integral n) => n -> Producer Text m r :~> Producer Text m (Producer Text m r)
splitAtT = PT.splitAt
{-# INLINE splitAtT #-}

splitsT :: (Monad m) => Char -> Producer Text m r :~> FreeT (Producer Text m) m r
splitsT = PT.splits
{-# INLINE splitsT #-}

splitsWithT :: (Monad m) => (Char -> Bool) -> Producer Text m r -> FreeT (Producer Text m) m r
splitsWithT = PT.splitsWith
{-# INLINE splitsWithT #-}

takeT :: (Monad m, Integral a) => a -> Pipe Text Text m ()
takeT = PT.take
{-# INLINE takeT #-}

takeWhileT :: (Monad m) => (Char -> Bool) -> Pipe Text Text m ()
takeWhileT = PT.takeWhile
{-# INLINE takeWhileT #-}

toCaseFoldT :: Monad m => Pipe Text Text m r
toCaseFoldT = PT.toCaseFold
{-# INLINE toCaseFoldT #-}

toLazyT :: Producer Text Identity () -> LText
toLazyT = PT.toLazy
{-# INLINE toLazyT #-}

toLazyMT :: (Monad m) => Producer Text m () -> m LText
toLazyMT = PT.toLazyM
{-# INLINE toLazyMT #-}

toLowerT :: Monad m => Pipe Text Text m r
toLowerT = PT.toLower
{-# INLINE toLowerT #-}

toUpperT :: Monad m => Pipe Text Text m r
toUpperT = PT.toUpper
{-# INLINE toUpperT #-}

unlinesT :: Monad m => FreeT (Producer Text m) m r :~> Producer Text m r
unlinesT = PT.unlines
{-# INLINE unlinesT #-}

unpackT :: Monad m => Producer Text m r :~> Producer Char m r
unpackT = PT.unpack
{-# INLINE unpackT #-}

unwordsT :: Monad m => FreeT (Producer Text m) m r :~> Producer Text m r
unwordsT = PT.unwords
{-# INLINE unwordsT #-}

wordT :: (Monad m) => Producer Text m r :~> Producer Text m (Producer Text m r)
wordT = PT.word
{-# INLINE wordT #-}

wordsT :: (Monad m) => Producer Text m r :~> FreeT (Producer Text m) m r
wordsT = PT.words
{-# INLINE wordsT #-}

