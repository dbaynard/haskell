{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, ApplicativeDo, PartialTypeSignatures #-}

module Forestay.Serial (
    module X
  , AesonOptions
  , CsvField
  , CsvParser
  , ProtoField
  , ProtoVal
  , YamlParser
  , encodeCSV
  , decodeCSV
  , encodeJSON
  , decodeJSON
  , encodeYaml
  , decodeYaml
  , encodeProtoBuf
  , decodeProtoBuf
)   where

import Forestay.Data
import Forestay.Serial.MessagePack as X

import qualified Data.Aeson as A
import Data.Aeson as X hiding
    ( encode
    , decode
    )
import qualified Data.Aeson.Types as A
import Data.Aeson.Types as X hiding
    ( Options
    )

import Data.Aeson.Lens as X

import qualified Data.Csv as C
import Data.Csv as X hiding
    ( encode
    , decode
    , Parser
    , Field
    , (.:)
    , (.=)
    )

import qualified Data.ProtocolBuffers as P
import Data.ProtocolBuffers as X hiding
    ( Value()
    , encode
    , decode
    , Field
    )

import Data.ProtocolBuffers.Internal
    ( Tag()
    , WireField()
    )

import qualified Data.Yaml as Y
import Data.Yaml as X hiding
    ( encode
    , decode
    , Parser
    , (.:)
    , (.=)
    )

import Data.HashMap.Strict
import Data.Serialize

type AesonOptions = A.Options

type CsvParser = C.Parser
type CsvField = C.Field

type ProtoVal = P.Value
type ProtoField = P.Field

type YamlParser = Y.Parser

encodeCSV :: ToRecord a => [a] -> LByteString
encodeCSV = C.encode
{-# INLINE encodeCSV #-}

decodeCSV :: FromRecord a => HasHeader -> LByteString -> Either String (Vector a)
decodeCSV = C.decode
{-# INLINE decodeCSV #-}

encodeJSON :: ToJSON a => a -> LByteString
encodeJSON = A.encode
{-# INLINE encodeJSON #-}

decodeJSON :: FromJSON a => LByteString -> Maybe a
decodeJSON = A.decode
{-# INLINE decodeJSON #-}

encodeYaml :: ToJSON a => a -> ByteString
encodeYaml = Y.encode
{-# INLINE encodeYaml #-}

decodeYaml :: FromJSON a => ByteString -> Maybe a
decodeYaml = Y.decode
{-# INLINE decodeYaml #-}

encodeProtoBuf :: Encode a => a -> Put
encodeProtoBuf = P.encode
{-# INLINE encodeProtoBuf #-}

decodeProtoBuf :: Decode a => HashMap Tag [WireField] -> Get a
decodeProtoBuf = P.decode
{-# INLINE decodeProtoBuf #-}

