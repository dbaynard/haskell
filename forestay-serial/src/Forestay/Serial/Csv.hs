{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, ApplicativeDo #-}
{-# LANGUAGE PartialTypeSignatures, TypeApplications, NamedWildCards #-}
{-# LANGUAGE PatternSynonyms #-}

module Forestay.Serial.Csv (
    module X
  , CsvParser
  , pattern CsvFail
  , pattern CsvMany
  , pattern CsvDone
  , CsvField
  , CsvBuilder
  , CsvIncParser
  , CsvRecords
  , pattern CsvCons
  , pattern CsvNil
  , csvEncodeL
  , csvEncodeWithL
  , csvEncodeByNameL
  , csvEncodeByNameWithL
  , csvEncodeDefaultOrderedByNameL
  , csvEncodeDefaultOrderedByNameWithL
  , csvDecodeL
  , csvDecodeWithL
  , csvDecodeByNameL
  , csvDecodeByNameWithL
  , csvEncodeWith
  , csvEncodeByName
  , csvEncodeByNameWith
  , csvEncodeDefaultOrderedByName
  , csvEncodeDefaultOrderedByNameWith
  , csvDecode
  , csvDecodeWith
  , csvDecodeByName
  , csvDecodeByNameWith
  , csvDecodeS
  , csvDecodeWithS
  , csvDecodeByNameS
  , csvDecodeByNameWithS
)   where

import Forestay.Data

import qualified Data.Csv as C
import Data.Csv as X hiding
    ( encode
    , encodeWith
    , encodeByName
    , encodeByNameWith
    , encodeDefaultOrderedByName
    , encodeDefaultOrderedByNameWith
    , decode
    , decodeWith
    , decodeByName
    , decodeByNameWith
    , Parser
    , Field
    , (.:)
    , (.=)
    , index
    )

import qualified Data.Csv.Incremental as I
import Data.Csv.Incremental as X hiding
    ( encodeWith
    , encodeByName
    , encodeByNameWith
    , encodeDefaultOrderedByName
    , encodeDefaultOrderedByNameWith
    , decode
    , decodeWith
    , decodeByName
    , decodeByNameWith
    , Builder
    , Parser
    , Fail
    , Many
    , Done
    )

import qualified Data.Csv.Streaming as S

type CsvParser = C.Parser
type CsvField = C.Field
type CsvBuilder = I.Builder
type CsvIncParser = I.Parser
type CsvRecords = S.Records

pattern CsvCons :: Either String t -> CsvRecords t -> CsvRecords t
pattern CsvCons esa r = S.Cons esa r

pattern CsvNil :: Maybe String -> LByteString -> CsvRecords t
pattern CsvNil ms b = S.Nil ms b

pattern CsvFail :: ByteString -> String -> CsvIncParser a
pattern CsvFail b s = I.Fail b s

pattern CsvMany :: [Either String a] -> (ByteString -> CsvIncParser a) -> CsvIncParser a
pattern CsvMany esa bpa = I.Many esa bpa

pattern CsvDone :: [Either String a] -> CsvIncParser a
pattern CsvDone esa = I.Done esa

--------------------------------------------------
-- * CSV encoding via lazy bytestring
--------------------------------------------------

csvEncodeL :: ToRecord a => [a] -> LByteString
csvEncodeL = C.encode
{-# INLINE csvEncodeL #-}

csvEncodeWithL :: ToRecord a => EncodeOptions -> [a] -> LByteString
csvEncodeWithL = C.encodeWith
{-# INLINE csvEncodeWithL #-}

csvEncodeByNameL :: ToNamedRecord a => Header -> [a] -> LByteString
csvEncodeByNameL = C.encodeByName
{-# INLINE csvEncodeByNameL #-}

csvEncodeByNameWithL :: ToNamedRecord a => EncodeOptions -> Header -> [a] -> LByteString
csvEncodeByNameWithL = C.encodeByNameWith
{-# INLINE csvEncodeByNameWithL #-}

csvEncodeDefaultOrderedByNameL :: (DefaultOrdered a, ToNamedRecord a) => [a] -> LByteString
csvEncodeDefaultOrderedByNameL = C.encodeDefaultOrderedByName
{-# INLINE csvEncodeDefaultOrderedByNameL #-}

csvEncodeDefaultOrderedByNameWithL :: (DefaultOrdered a, ToNamedRecord a) => EncodeOptions -> [a] -> LByteString
csvEncodeDefaultOrderedByNameWithL = C.encodeDefaultOrderedByNameWith
{-# INLINE csvEncodeDefaultOrderedByNameWithL #-}

csvDecodeL :: FromRecord a => HasHeader -> LByteString -> Either String (Vector a)
csvDecodeL = C.decode
{-# INLINE csvDecodeL #-}

csvDecodeWithL :: FromRecord a => DecodeOptions -> HasHeader -> LByteString -> Either String (Vector a)
csvDecodeWithL = C.decodeWith
{-# INLINE csvDecodeWithL #-}

csvDecodeByNameL :: FromNamedRecord a => LByteString -> Either String (Header, Vector a)
csvDecodeByNameL = C.decodeByName
{-# INLINE csvDecodeByNameL #-}

csvDecodeByNameWithL :: FromNamedRecord a => DecodeOptions -> LByteString -> Either String (Header, Vector a)
csvDecodeByNameWithL = C.decodeByNameWith
{-# INLINE csvDecodeByNameWithL #-}

--------------------------------------------------
-- * CSV encoding via incremental parser
--------------------------------------------------

csvEncodeWith :: ToRecord a => EncodeOptions -> CsvBuilder a -> LByteString
csvEncodeWith = I.encodeWith
{-# INLINE csvEncodeWith #-}

csvEncodeByName :: ToNamedRecord a => Header -> NamedBuilder a -> LByteString
csvEncodeByName = I.encodeByName
{-# INLINE csvEncodeByName #-}

csvEncodeByNameWith :: ToNamedRecord a => EncodeOptions -> Header -> NamedBuilder a -> LByteString
csvEncodeByNameWith = I.encodeByNameWith
{-# INLINE csvEncodeByNameWith #-}

csvEncodeDefaultOrderedByName :: (DefaultOrdered a, ToNamedRecord a) => NamedBuilder a -> LByteString
csvEncodeDefaultOrderedByName = I.encodeDefaultOrderedByName
{-# INLINE csvEncodeDefaultOrderedByName #-}

csvEncodeDefaultOrderedByNameWith :: (DefaultOrdered a, ToNamedRecord a) => EncodeOptions -> NamedBuilder a -> LByteString
csvEncodeDefaultOrderedByNameWith = I.encodeDefaultOrderedByNameWith
{-# INLINE csvEncodeDefaultOrderedByNameWith #-}

csvDecode :: FromRecord a => HasHeader -> CsvIncParser a
csvDecode = I.decode
{-# INLINE csvDecode #-}

csvDecodeWith :: FromRecord a => DecodeOptions -> HasHeader -> CsvIncParser a
csvDecodeWith = I.decodeWith
{-# INLINE csvDecodeWith #-}

csvDecodeByName :: FromNamedRecord a => HeaderParser (CsvIncParser a)
csvDecodeByName = I.decodeByName
{-# INLINE csvDecodeByName #-}

csvDecodeByNameWith :: FromNamedRecord a => DecodeOptions -> HeaderParser (CsvIncParser a)
csvDecodeByNameWith = I.decodeByNameWith
{-# INLINE csvDecodeByNameWith #-}

--------------------------------------------------
-- * CSV decoding only via the streaming decoder
--------------------------------------------------

csvDecodeS :: FromRecord a => HasHeader -> LByteString -> CsvRecords a
csvDecodeS = S.decode
{-# INLINE csvDecodeS #-}

csvDecodeWithS :: FromRecord a => DecodeOptions -> HasHeader -> LByteString -> CsvRecords a
csvDecodeWithS = S.decodeWith
{-# INLINE csvDecodeWithS #-}

csvDecodeByNameS :: FromNamedRecord a => LByteString -> Either String (Header, CsvRecords a)
csvDecodeByNameS = S.decodeByName
{-# INLINE csvDecodeByNameS #-}

csvDecodeByNameWithS :: FromNamedRecord a => DecodeOptions -> LByteString -> Either String (Header, CsvRecords a)
csvDecodeByNameWithS = S.decodeByNameWith
{-# INLINE csvDecodeByNameWithS #-}
