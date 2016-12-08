{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, ApplicativeDo, PartialTypeSignatures #-}

module Forestay.Serial (
    module X
  , MPObject
  , ProtoVal
  , AesonOptions
  , encodeJSON
  , decodeJSON
  , encodeYaml
  , decodeYaml
  , encodeProtoBuf
  , decodeProtoBuf
)   where

import Forestay.Data

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

import qualified Data.MessagePack as M
import Data.MessagePack as X hiding
    ( Object()
    )

import qualified Data.ProtocolBuffers as P
import Data.ProtocolBuffers as X hiding
    ( Value()
    , encode
    , decode
    )

import Data.ProtocolBuffers.Internal
    ( Tag()
    , WireField()
    )

import qualified Data.Yaml as Y
import Data.Yaml as X hiding
    ( encode
    , decode
    )

import Data.HashMap.Strict
import Data.Serialize

type MPObject = M.Object

type ProtoVal = P.Value

type AesonOptions = A.Options

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

decodeProtoBuf :: Decode a => HashMap Tag [WireField] -> Get a
decodeProtoBuf = P.decode


