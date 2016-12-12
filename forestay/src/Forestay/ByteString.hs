{-# LANGUAGE NoImplicitPrelude #-}

module Forestay.ByteString (
    module X
  , allSB
  , anySB
  , appendSB
  , appendFileSB
  , breakSB
  , concatSB
  , concatMapSB
  , consSB
  , copySB
  , countSB
  , dropSB
  , dropWhileSB
  , elemSB
  , elemIndexSB
  , elemIndexEndSB
  , elemIndicesSB
  , emptySB
  , filterSB
  , findSB
  , findIndexSB
  , findIndicesSB
  , foldlSB
  , foldlSB'
  , foldl1SB
  , foldl1SB'
  , foldrSB
  , foldr1SB
  , getContentsSB
  , groupSB
  , groupBySB
  , hGetSB
  , hGetContentsSB
  , hGetNonBlockingSB
  , hPutSB
  , hPutNonBlockingSB
  , hPutStrSB
  , headSB
  , indexSB
  , initSB
  , initsSB
  , interactSB
  , intercalateSB
  , intersperseSB
  , isPrefixOfSB
  , isSuffixOfSB
  , lastSB
  , lengthSB
  , mapSB
  , mapAccumLSB
  , mapAccumRSB
  , maximumSB
  , minimumSB
  , notElemSB
  , nullSB
  , packSB
  , partitionSB
  , putStrSB
  , readFileSB
  , replicateSB
  , reverseSB
  , scanlSB
  , singletonSB
  , snocSB
  , spanSB
  , splitSB
  , splitAtSB
  , splitWithSB
  , stripPrefixSB
  , stripSuffixSB
  , tailSB
  , tailsSB
  , takeSB
  , takeWhileSB
  , transposeSB
  , unconsSB
  , unfoldrSB
  , unpackSB
  , unsnocSB
  , unzipSB
  , writeFileSB
  , zipSB
  , zipWithSB
  , getLineSB
  , sortSB
  , scanrSB
  , foldrSB'
  , hGetSomeSB
  , hGetLineSB
  , allLB
  , anyLB
  , appendLB
  , appendFileLB
  , breakLB
  , concatLB
  , concatMapLB
  , consLB
  , copyLB
  , countLB
  , dropLB
  , dropWhileLB
  , elemLB
  , elemIndexLB
  , elemIndexEndLB
  , elemIndicesLB
  , emptyLB
  , filterLB
  , findLB
  , findIndexLB
  , findIndicesLB
  , foldlLB
  , foldlLB'
  , foldl1LB
  , foldl1LB'
  , foldrLB
  , foldr1LB
  , getContentsLB
  , groupLB
  , groupByLB
  , hGetLB
  , hGetContentsLB
  , hGetNonBlockingLB
  , hPutLB
  , hPutNonBlockingLB
  , hPutStrLB
  , headLB
  , indexLB
  , initLB
  , initsLB
  , interactLB
  , intercalateLB
  , intersperseLB
  , isPrefixOfLB
  , isSuffixOfLB
  , lastLB
  , lengthLB
  , mapLB
  , mapAccumLLB
  , mapAccumRLB
  , maximumLB
  , minimumLB
  , notElemLB
  , nullLB
  , packLB
  , partitionLB
  , putStrLB
  , readFileLB
  , replicateLB
  , reverseLB
  , scanlLB
  , singletonLB
  , snocLB
  , spanLB
  , splitLB
  , splitAtLB
  , splitWithLB
  , stripPrefixLB
  , stripSuffixLB
  , tailLB
  , tailsLB
  , takeLB
  , takeWhileLB
  , transposeLB
  , unconsLB
  , unfoldrLB
  , unpackLB
  , unsnocLB
  , unzipLB
  , writeFileLB
  , zipLB
  , zipWithLB
  , cycleLB
  , iterateLB
  , repeatLB
  , fromStrictByteString
  , toStrictByteString
)   where

import Forestay.Data

import Data.ByteString.Builder as X

import qualified Data.ByteString as Strict
import Data.ByteString as X hiding
    ( ByteString
    , all
    , any
    , append
    , appendFile
    , break
    , concat
    , concatMap
    , cons
    , copy
    , count
    , drop
    , dropWhile
    , elem
    , elemIndex
    , elemIndexEnd
    , elemIndices
    , empty
    , filter
    , find
    , findIndex
    , findIndices
    , foldl
    , foldl'
    , foldl1
    , foldl1'
    , foldr
    , foldr1
    , getContents
    , group
    , groupBy
    , hGet
    , hGetContents
    , hGetNonBlocking
    , hPut
    , hPutNonBlocking
    , hPutStr
    , hPutStrLn
    , head
    , index
    , init
    , inits
    , interact
    , intercalate
    , intersperse
    , isPrefixOf
    , isSuffixOf
    , last
    , length
    , map
    , mapAccumL
    , mapAccumR
    , maximum
    , minimum
    , notElem
    , null
    , pack
    , partition
    , putStr
    , putStrLn
    , readFile
    , replicate
    , reverse
    , scanl
    , singleton
    , snoc
    , span
    , split
    , splitAt
    , splitWith
    , stripPrefix
    , stripSuffix
    , tail
    , tails
    , take
    , takeWhile
    , transpose
    , uncons
    , unfoldr
    , unpack
    , unpack
    , unsnoc
    , unzip
    , unzip
    , writeFile
    , zip
    , zipWith
    -- conflicts with Forestay
    , getLine
    , sort
    , scanr
    , foldr'
    , hGetSome
    , hGetLine
    )

import qualified Data.ByteString.Lazy as Lazy
import Data.ByteString.Lazy as X hiding
    ( ByteString
    , all
    , any
    , append
    , appendFile
    , break
    , concat
    , concatMap
    , cons
    , copy
    , count
    , drop
    , dropWhile
    , elem
    , elemIndex
    , elemIndexEnd
    , elemIndices
    , empty
    , filter
    , find
    , findIndex
    , findIndices
    , foldl
    , foldl'
    , foldl1
    , foldl1'
    , foldr
    , foldr1
    , getContents
    , group
    , groupBy
    , hGet
    , hGetContents
    , hGetNonBlocking
    , hPut
    , hPutNonBlocking
    , hPutStr
    , head
    , index
    , init
    , inits
    , interact
    , intercalate
    , intersperse
    , isPrefixOf
    , isSuffixOf
    , last
    , length
    , map
    , mapAccumL
    , mapAccumR
    , maximum
    , minimum
    , notElem
    , null
    , pack
    , partition
    , putStr
    , putStrLn
    , readFile
    , replicate
    , reverse
    , scanl
    , singleton
    , snoc
    , span
    , split
    , splitAt
    , splitWith
    , stripPrefix
    , stripSuffix
    , tail
    , tails
    , take
    , takeWhile
    , transpose
    , uncons
    , unfoldr
    , unpack
    , unpack
    , unsnoc
    , unzip
    , unzip
    , writeFile
    , zip
    , zipWith
    -- conflicts with Forestay
    , cycle
    , iterate
    , repeat
    , fromStrict
    , toStrict
    )

--------------------------------------------------
-- * Strict
--------------------------------------------------

allSB :: (Word8 -> Bool) -> ByteString -> Bool
allSB = Strict.all
{-# INLINE allSB #-}

anySB :: (Word8 -> Bool) -> ByteString -> Bool
anySB = Strict.any
{-# INLINE anySB #-}

appendSB :: ByteString -> ByteString -> ByteString
appendSB = Strict.append
{-# INLINE appendSB #-}

appendFileSB :: FilePath -> ByteString -> IO ()
appendFileSB = Strict.appendFile
{-# INLINE appendFileSB #-}

breakSB :: (Word8 -> Bool)
           -> ByteString -> (ByteString, ByteString)
breakSB = Strict.break
{-# INLINE breakSB #-}

concatSB :: [ByteString] -> ByteString
concatSB = Strict.concat
{-# INLINE concatSB #-}

concatMapSB :: (Word8 -> ByteString) -> ByteString -> ByteString
concatMapSB = Strict.concatMap
{-# INLINE concatMapSB #-}

consSB :: Word8 -> ByteString -> ByteString
consSB = Strict.cons
{-# INLINE consSB #-}

copySB :: ByteString -> ByteString
copySB = Strict.copy
{-# INLINE copySB #-}

countSB :: Word8 -> ByteString -> Int
countSB = Strict.count
{-# INLINE countSB #-}

dropSB :: Int -> ByteString -> ByteString
dropSB = Strict.drop
{-# INLINE dropSB #-}

dropWhileSB :: (Word8 -> Bool) -> ByteString -> ByteString
dropWhileSB = Strict.dropWhile
{-# INLINE dropWhileSB #-}

elemSB :: Word8 -> ByteString -> Bool
elemSB = Strict.elem
{-# INLINE elemSB #-}

elemIndexSB :: Word8 -> ByteString -> Maybe Int
elemIndexSB = Strict.elemIndex
{-# INLINE elemIndexSB #-}

elemIndexEndSB :: Word8 -> ByteString -> Maybe Int
elemIndexEndSB = Strict.elemIndexEnd
{-# INLINE elemIndexEndSB #-}

elemIndicesSB :: Word8 -> ByteString -> [Int]
elemIndicesSB = Strict.elemIndices
{-# INLINE elemIndicesSB #-}

emptySB :: ByteString
emptySB = Strict.empty
{-# INLINE emptySB #-}

filterSB :: (Word8 -> Bool) -> ByteString -> ByteString
filterSB = Strict.filter
{-# INLINE filterSB #-}

findSB :: (Word8 -> Bool) -> ByteString -> Maybe Word8
findSB = Strict.find
{-# INLINE findSB #-}

findIndexSB :: (Word8 -> Bool) -> ByteString -> Maybe Int
findIndexSB = Strict.findIndex
{-# INLINE findIndexSB #-}

findIndicesSB :: (Word8 -> Bool) -> ByteString -> [Int]
findIndicesSB = Strict.findIndices
{-# INLINE findIndicesSB #-}

foldlSB :: (a -> Word8 -> a) -> a -> ByteString -> a
foldlSB = Strict.foldl
{-# INLINE foldlSB #-}

foldlSB' :: (a -> Word8 -> a) -> a -> ByteString -> a
foldlSB' = Strict.foldl'
{-# INLINE foldlSB' #-}

foldl1SB :: (Word8 -> Word8 -> Word8) -> ByteString -> Word8
foldl1SB = Strict.foldl1
{-# INLINE foldl1SB #-}

foldl1SB' :: (Word8 -> Word8 -> Word8) -> ByteString -> Word8
foldl1SB' = Strict.foldl1'
{-# INLINE foldl1SB' #-}

foldrSB :: (Word8 -> a -> a) -> a -> ByteString -> a
foldrSB = Strict.foldr
{-# INLINE foldrSB #-}

foldr1SB :: (Word8 -> Word8 -> Word8) -> ByteString -> Word8
foldr1SB = Strict.foldr1
{-# INLINE foldr1SB #-}

getContentsSB :: IO ByteString
getContentsSB = Strict.getContents
{-# INLINE getContentsSB #-}

groupSB :: ByteString -> [ByteString]
groupSB = Strict.group
{-# INLINE groupSB #-}

groupBySB :: (Word8 -> Word8 -> Bool) -> ByteString -> [ByteString]
groupBySB = Strict.groupBy
{-# INLINE groupBySB #-}

hGetSB :: Handle -> Int -> IO ByteString
hGetSB = Strict.hGet
{-# INLINE hGetSB #-}

hGetContentsSB :: Handle -> IO ByteString
hGetContentsSB = Strict.hGetContents
{-# INLINE hGetContentsSB #-}

hGetNonBlockingSB :: Handle -> Int -> IO ByteString
hGetNonBlockingSB = Strict.hGetNonBlocking
{-# INLINE hGetNonBlockingSB #-}

hPutSB :: Handle -> ByteString -> IO ()
hPutSB = Strict.hPut
{-# INLINE hPutSB #-}

hPutNonBlockingSB :: Handle -> ByteString -> IO ByteString
hPutNonBlockingSB = Strict.hPutNonBlocking
{-# INLINE hPutNonBlockingSB #-}

hPutStrSB :: Handle -> ByteString -> IO ()
hPutStrSB = Strict.hPutStr
{-# INLINE hPutStrSB #-}

headSB :: ByteString -> Word8
headSB = Strict.head
{-# INLINE headSB #-}

indexSB :: ByteString -> Int -> Word8
indexSB = Strict.index
{-# INLINE indexSB #-}

initSB :: ByteString -> ByteString
initSB = Strict.init
{-# INLINE initSB #-}

initsSB :: ByteString -> [ByteString]
initsSB = Strict.inits
{-# INLINE initsSB #-}

interactSB :: (ByteString -> ByteString) -> IO ()
interactSB = Strict.interact
{-# INLINE interactSB #-}

intercalateSB :: ByteString -> [ByteString] -> ByteString
intercalateSB = Strict.intercalate
{-# INLINE intercalateSB #-}

intersperseSB :: Word8 -> ByteString -> ByteString
intersperseSB = Strict.intersperse
{-# INLINE intersperseSB #-}

isPrefixOfSB :: ByteString -> ByteString -> Bool
isPrefixOfSB = Strict.isPrefixOf
{-# INLINE isPrefixOfSB #-}

isSuffixOfSB :: ByteString -> ByteString -> Bool
isSuffixOfSB = Strict.isSuffixOf
{-# INLINE isSuffixOfSB #-}

lastSB :: ByteString -> Word8
lastSB = Strict.last
{-# INLINE lastSB #-}

lengthSB :: ByteString -> Int
lengthSB = Strict.length
{-# INLINE lengthSB #-}

mapSB :: (Word8 -> Word8) -> ByteString -> ByteString
mapSB = Strict.map
{-# INLINE mapSB #-}

mapAccumLSB :: (acc -> Word8 -> (acc, Word8))
               -> acc -> ByteString -> (acc, ByteString)
mapAccumLSB = Strict.mapAccumL
{-# INLINE mapAccumLSB #-}

mapAccumRSB :: (acc -> Word8 -> (acc, Word8))
               -> acc -> ByteString -> (acc, ByteString)
mapAccumRSB = Strict.mapAccumR
{-# INLINE mapAccumRSB #-}

maximumSB :: ByteString -> Word8
maximumSB = Strict.maximum
{-# INLINE maximumSB #-}

minimumSB :: ByteString -> Word8
minimumSB = Strict.minimum
{-# INLINE minimumSB #-}

notElemSB :: Word8 -> ByteString -> Bool
notElemSB = Strict.notElem
{-# INLINE notElemSB #-}

nullSB :: ByteString -> Bool
nullSB = Strict.null
{-# INLINE nullSB #-}

packSB :: [Word8] -> ByteString
packSB = Strict.pack
{-# INLINE packSB #-}

partitionSB :: (Word8 -> Bool)
               -> ByteString -> (ByteString, ByteString)
partitionSB = Strict.partition
{-# INLINE partitionSB #-}

putStrSB :: ByteString -> IO ()
putStrSB = Strict.putStr
{-# INLINE putStrSB #-}

readFileSB :: FilePath -> IO ByteString
readFileSB = Strict.readFile
{-# INLINE readFileSB #-}

replicateSB :: Int -> Word8 -> ByteString
replicateSB = Strict.replicate
{-# INLINE replicateSB #-}

reverseSB :: ByteString -> ByteString
reverseSB = Strict.reverse
{-# INLINE reverseSB #-}

scanlSB :: (Word8 -> Word8 -> Word8)
           -> Word8 -> ByteString -> ByteString
scanlSB = Strict.scanl
{-# INLINE scanlSB #-}

singletonSB :: Word8 -> ByteString
singletonSB = Strict.singleton
{-# INLINE singletonSB #-}

snocSB :: ByteString -> Word8 -> ByteString
snocSB = Strict.snoc
{-# INLINE snocSB #-}

spanSB :: (Word8 -> Bool) -> ByteString -> (ByteString, ByteString)
spanSB = Strict.span
{-# INLINE spanSB #-}

splitSB :: Word8 -> ByteString -> [ByteString]
splitSB = Strict.split
{-# INLINE splitSB #-}

splitAtSB :: Int -> ByteString -> (ByteString, ByteString)
splitAtSB = Strict.splitAt
{-# INLINE splitAtSB #-}

splitWithSB :: (Word8 -> Bool) -> ByteString -> [ByteString]
splitWithSB = Strict.splitWith
{-# INLINE splitWithSB #-}

stripPrefixSB :: ByteString -> ByteString -> Maybe ByteString
stripPrefixSB = Strict.stripPrefix
{-# INLINE stripPrefixSB #-}

stripSuffixSB :: ByteString -> ByteString -> Maybe ByteString
stripSuffixSB = Strict.stripSuffix
{-# INLINE stripSuffixSB #-}

tailSB :: ByteString -> ByteString
tailSB = Strict.tail
{-# INLINE tailSB #-}

tailsSB :: ByteString -> [ByteString]
tailsSB = Strict.tails
{-# INLINE tailsSB #-}

takeSB :: Int -> ByteString -> ByteString
takeSB = Strict.take
{-# INLINE takeSB #-}

takeWhileSB :: (Word8 -> Bool) -> ByteString -> ByteString
takeWhileSB = Strict.takeWhile
{-# INLINE takeWhileSB #-}

transposeSB :: [ByteString] -> [ByteString]
transposeSB = Strict.transpose
{-# INLINE transposeSB #-}

unconsSB :: ByteString -> Maybe (Word8, ByteString)
unconsSB = Strict.uncons
{-# INLINE unconsSB #-}

unfoldrSB :: (a -> Maybe (Word8, a)) -> a -> ByteString
unfoldrSB = Strict.unfoldr
{-# INLINE unfoldrSB #-}

unpackSB :: ByteString -> [Word8]
unpackSB = Strict.unpack
{-# INLINE unpackSB #-}

unsnocSB :: ByteString -> Maybe (ByteString, Word8)
unsnocSB = Strict.unsnoc
{-# INLINE unsnocSB #-}

unzipSB :: [(Word8, Word8)] -> (ByteString, ByteString)
unzipSB = Strict.unzip
{-# INLINE unzipSB #-}

writeFileSB :: FilePath -> ByteString -> IO ()
writeFileSB = Strict.writeFile
{-# INLINE writeFileSB #-}

zipSB :: ByteString -> ByteString -> [(Word8, Word8)]
zipSB = Strict.zip
{-# INLINE zipSB #-}

zipWithSB :: (Word8 -> Word8 -> a) -> ByteString -> ByteString -> [a]
zipWithSB = Strict.zipWith
{-# INLINE zipWithSB #-}

--------------------------------------------------
-- ** Conflicts with Forestay
--------------------------------------------------

getLineSB :: IO ByteString
getLineSB = Strict.getLine
{-# INLINE getLineSB #-}

sortSB :: ByteString -> ByteString
sortSB = Strict.sort
{-# INLINE sortSB #-}

scanrSB :: (Word8 -> Word8 -> Word8)
           -> Word8 -> ByteString -> ByteString
scanrSB = Strict.scanr
{-# INLINE scanrSB #-}

foldrSB' :: (Word8 -> a -> a) -> a -> ByteString -> a
foldrSB' = Strict.foldr'
{-# INLINE foldrSB' #-}

hGetSomeSB :: Handle -> Int -> IO ByteString
hGetSomeSB = Strict.hGetSome
{-# INLINE hGetSomeSB #-}

hGetLineSB :: Handle -> IO ByteString
hGetLineSB = Strict.hGetLine
{-# INLINE hGetLineSB #-}

--------------------------------------------------
-- * Lazy
--------------------------------------------------

allLB :: (Word8 -> Bool) -> LByteString -> Bool
allLB = Lazy.all
{-# INLINE allLB #-}

anyLB :: (Word8 -> Bool) -> LByteString -> Bool
anyLB = Lazy.any
{-# INLINE anyLB #-}

appendLB :: LByteString -> LByteString -> LByteString
appendLB = Lazy.append
{-# INLINE appendLB #-}

appendFileLB :: FilePath -> LByteString -> IO ()
appendFileLB = Lazy.appendFile
{-# INLINE appendFileLB #-}

breakLB :: (Word8 -> Bool)
           -> LByteString -> (LByteString, LByteString)
breakLB = Lazy.break
{-# INLINE breakLB #-}

concatLB :: [LByteString] -> LByteString
concatLB = Lazy.concat
{-# INLINE concatLB #-}

concatMapLB :: (Word8 -> LByteString)
               -> LByteString -> LByteString
concatMapLB = Lazy.concatMap
{-# INLINE concatMapLB #-}

consLB :: Word8 -> LByteString -> LByteString
consLB = Lazy.cons
{-# INLINE consLB #-}

copyLB :: LByteString -> LByteString
copyLB = Lazy.copy
{-# INLINE copyLB #-}

countLB :: Word8 -> LByteString -> Int64
countLB = Lazy.count
{-# INLINE countLB #-}

dropLB :: Int64 -> LByteString -> LByteString
dropLB = Lazy.drop
{-# INLINE dropLB #-}

dropWhileLB :: (Word8 -> Bool)
               -> LByteString -> LByteString
dropWhileLB = Lazy.dropWhile
{-# INLINE dropWhileLB #-}

elemLB :: Word8 -> LByteString -> Bool
elemLB = Lazy.elem
{-# INLINE elemLB #-}

elemIndexLB :: Word8 -> LByteString -> Maybe Int64
elemIndexLB = Lazy.elemIndex
{-# INLINE elemIndexLB #-}

elemIndexEndLB :: Word8 -> LByteString -> Maybe Int64
elemIndexEndLB = Lazy.elemIndexEnd
{-# INLINE elemIndexEndLB #-}

elemIndicesLB :: Word8 -> LByteString -> [Int64]
elemIndicesLB = Lazy.elemIndices
{-# INLINE elemIndicesLB #-}

emptyLB :: LByteString
emptyLB = Lazy.empty
{-# INLINE emptyLB #-}

filterLB :: (Word8 -> Bool) -> LByteString -> LByteString
filterLB = Lazy.filter
{-# INLINE filterLB #-}

findLB :: (Word8 -> Bool) -> LByteString -> Maybe Word8
findLB = Lazy.find
{-# INLINE findLB #-}

findIndexLB :: (Word8 -> Bool) -> LByteString -> Maybe Int64
findIndexLB = Lazy.findIndex
{-# INLINE findIndexLB #-}

findIndicesLB :: (Word8 -> Bool) -> LByteString -> [Int64]
findIndicesLB = Lazy.findIndices
{-# INLINE findIndicesLB #-}

foldlLB :: (a -> Word8 -> a) -> a -> LByteString -> a
foldlLB = Lazy.foldl
{-# INLINE foldlLB #-}

foldlLB' :: (a -> Word8 -> a) -> a -> LByteString -> a
foldlLB' = Lazy.foldl'
{-# INLINE foldlLB' #-}

foldl1LB :: (Word8 -> Word8 -> Word8) -> LByteString -> Word8
foldl1LB = Lazy.foldl1
{-# INLINE foldl1LB #-}

foldl1LB' :: (Word8 -> Word8 -> Word8) -> LByteString -> Word8
foldl1LB' = Lazy.foldl1'
{-# INLINE foldl1LB' #-}

foldrLB :: (Word8 -> a -> a) -> a -> LByteString -> a
foldrLB = Lazy.foldr
{-# INLINE foldrLB #-}

foldr1LB :: (Word8 -> Word8 -> Word8) -> LByteString -> Word8
foldr1LB = Lazy.foldr1
{-# INLINE foldr1LB #-}

getContentsLB :: IO LByteString
getContentsLB = Lazy.getContents
{-# INLINE getContentsLB #-}

groupLB :: LByteString -> [LByteString]
groupLB = Lazy.group
{-# INLINE groupLB #-}

groupByLB :: (Word8 -> Word8 -> Bool)
             -> LByteString -> [LByteString]
groupByLB = Lazy.groupBy
{-# INLINE groupByLB #-}

hGetLB :: Handle -> Int -> IO LByteString
hGetLB = Lazy.hGet
{-# INLINE hGetLB #-}

hGetContentsLB :: Handle -> IO LByteString
hGetContentsLB = Lazy.hGetContents
{-# INLINE hGetContentsLB #-}

hGetNonBlockingLB :: Handle -> Int -> IO LByteString
hGetNonBlockingLB = Lazy.hGetNonBlocking
{-# INLINE hGetNonBlockingLB #-}

hPutLB :: Handle -> LByteString -> IO ()
hPutLB = Lazy.hPut
{-# INLINE hPutLB #-}

hPutNonBlockingLB :: Handle
                     -> LByteString -> IO LByteString
hPutNonBlockingLB = Lazy.hPutNonBlocking
{-# INLINE hPutNonBlockingLB #-}

hPutStrLB :: Handle -> LByteString -> IO ()
hPutStrLB = Lazy.hPutStr
{-# INLINE hPutStrLB #-}

headLB :: LByteString -> Word8
headLB = Lazy.head
{-# INLINE headLB #-}

indexLB :: LByteString -> Int64 -> Word8
indexLB = Lazy.index
{-# INLINE indexLB #-}

initLB :: LByteString -> LByteString
initLB = Lazy.init
{-# INLINE initLB #-}

initsLB :: LByteString -> [LByteString]
initsLB = Lazy.inits
{-# INLINE initsLB #-}

interactLB :: (LByteString -> LByteString) -> IO ()
interactLB = Lazy.interact
{-# INLINE interactLB #-}

intercalateLB :: LByteString
                 -> [LByteString] -> LByteString
intercalateLB = Lazy.intercalate
{-# INLINE intercalateLB #-}

intersperseLB :: Word8 -> LByteString -> LByteString
intersperseLB = Lazy.intersperse
{-# INLINE intersperseLB #-}

isPrefixOfLB :: LByteString -> LByteString -> Bool
isPrefixOfLB = Lazy.isPrefixOf
{-# INLINE isPrefixOfLB #-}

isSuffixOfLB :: LByteString -> LByteString -> Bool
isSuffixOfLB = Lazy.isSuffixOf
{-# INLINE isSuffixOfLB #-}

lastLB :: LByteString -> Word8
lastLB = Lazy.last
{-# INLINE lastLB #-}

lengthLB :: LByteString -> Int64
lengthLB = Lazy.length
{-# INLINE lengthLB #-}

mapLB :: (Word8 -> Word8) -> LByteString -> LByteString
mapLB = Lazy.map
{-# INLINE mapLB #-}

mapAccumLLB :: (acc -> Word8 -> (acc, Word8))
               -> acc -> LByteString -> (acc, LByteString)
mapAccumLLB = Lazy.mapAccumL
{-# INLINE mapAccumLLB #-}

mapAccumRLB :: (acc -> Word8 -> (acc, Word8))
               -> acc -> LByteString -> (acc, LByteString)
mapAccumRLB = Lazy.mapAccumR
{-# INLINE mapAccumRLB #-}

maximumLB :: LByteString -> Word8
maximumLB = Lazy.maximum
{-# INLINE maximumLB #-}

minimumLB :: LByteString -> Word8
minimumLB = Lazy.minimum
{-# INLINE minimumLB #-}

notElemLB :: Word8 -> LByteString -> Bool
notElemLB = Lazy.notElem
{-# INLINE notElemLB #-}

nullLB :: LByteString -> Bool
nullLB = Lazy.null
{-# INLINE nullLB #-}

packLB :: [Word8] -> LByteString
packLB = Lazy.pack
{-# INLINE packLB #-}

partitionLB :: (Word8 -> Bool)
               -> LByteString -> (LByteString, LByteString)
partitionLB = Lazy.partition
{-# INLINE partitionLB #-}

putStrLB :: LByteString -> IO ()
putStrLB = Lazy.putStr
{-# INLINE putStrLB #-}

readFileLB :: FilePath -> IO LByteString
readFileLB = Lazy.readFile
{-# INLINE readFileLB #-}

replicateLB :: Int64 -> Word8 -> LByteString
replicateLB = Lazy.replicate
{-# INLINE replicateLB #-}

reverseLB :: LByteString -> LByteString
reverseLB = Lazy.reverse
{-# INLINE reverseLB #-}

scanlLB :: (Word8 -> Word8 -> Word8)
           -> Word8 -> LByteString -> LByteString
scanlLB = Lazy.scanl
{-# INLINE scanlLB #-}

singletonLB :: Word8 -> LByteString
singletonLB = Lazy.singleton
{-# INLINE singletonLB #-}

snocLB :: LByteString -> Word8 -> LByteString
snocLB = Lazy.snoc
{-# INLINE snocLB #-}

spanLB :: (Word8 -> Bool)
          -> LByteString -> (LByteString, LByteString)
spanLB = Lazy.span
{-# INLINE spanLB #-}

splitLB :: Word8 -> LByteString -> [LByteString]
splitLB = Lazy.split
{-# INLINE splitLB #-}

splitAtLB :: Int64
             -> LByteString -> (LByteString, LByteString)
splitAtLB = Lazy.splitAt
{-# INLINE splitAtLB #-}

splitWithLB :: (Word8 -> Bool)
               -> LByteString -> [LByteString]
splitWithLB = Lazy.splitWith
{-# INLINE splitWithLB #-}

stripPrefixLB :: LByteString
                 -> LByteString -> Maybe LByteString
stripPrefixLB = Lazy.stripPrefix
{-# INLINE stripPrefixLB #-}

stripSuffixLB :: LByteString
                 -> LByteString -> Maybe LByteString
stripSuffixLB = Lazy.stripSuffix
{-# INLINE stripSuffixLB #-}

tailLB :: LByteString -> LByteString
tailLB = Lazy.tail
{-# INLINE tailLB #-}

tailsLB :: LByteString -> [LByteString]
tailsLB = Lazy.tails
{-# INLINE tailsLB #-}

takeLB :: Int64 -> LByteString -> LByteString
takeLB = Lazy.take
{-# INLINE takeLB #-}

takeWhileLB :: (Word8 -> Bool)
               -> LByteString -> LByteString
takeWhileLB = Lazy.takeWhile
{-# INLINE takeWhileLB #-}

transposeLB :: [LByteString] -> [LByteString]
transposeLB = Lazy.transpose
{-# INLINE transposeLB #-}

unconsLB :: LByteString -> Maybe (Word8, LByteString)
unconsLB = Lazy.uncons
{-# INLINE unconsLB #-}

unfoldrLB :: (a -> Maybe (Word8, a)) -> a -> LByteString
unfoldrLB = Lazy.unfoldr
{-# INLINE unfoldrLB #-}

unpackLB :: LByteString -> [Word8]
unpackLB = Lazy.unpack
{-# INLINE unpackLB #-}

unsnocLB :: LByteString -> Maybe (LByteString, Word8)
unsnocLB = Lazy.unsnoc
{-# INLINE unsnocLB #-}

unzipLB :: [(Word8, Word8)] -> (LByteString, LByteString)
unzipLB = Lazy.unzip
{-# INLINE unzipLB #-}

writeFileLB :: FilePath -> LByteString -> IO ()
writeFileLB = Lazy.writeFile
{-# INLINE writeFileLB #-}

zipLB :: LByteString -> LByteString -> [(Word8, Word8)]
zipLB = Lazy.zip
{-# INLINE zipLB #-}

zipWithLB :: (Word8 -> Word8 -> a) -> LByteString -> LByteString -> [a]
zipWithLB = Lazy.zipWith
{-# INLINE zipWithLB #-}

--------------------------------------------------
-- ** Conflicts with Forestay
--------------------------------------------------

cycleLB :: LByteString -> LByteString
cycleLB = Lazy.cycle
{-# INLINE cycleLB #-}

iterateLB :: (Word8 -> Word8) -> Word8 -> LByteString
iterateLB = Lazy.iterate
{-# INLINE iterateLB #-}

repeatLB :: Word8 -> LByteString
repeatLB = Lazy.repeat
{-# INLINE repeatLB #-}

fromStrictByteString :: ByteString -> LByteString
fromStrictByteString = Lazy.fromStrict
{-# INLINE fromStrictByteString #-}

toStrictByteString :: LByteString -> ByteString
toStrictByteString = Lazy.toStrict
{-# INLINE toStrictByteString #-}

