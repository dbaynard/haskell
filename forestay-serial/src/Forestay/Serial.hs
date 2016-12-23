{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, ApplicativeDo, PartialTypeSignatures #-}
{-# LANGUAGE FlexibleContexts #-}

module Forestay.Serial (
    module X
  , AesonOptions
  , AesonParser
  , ProtoField
  , ProtoVal
  , YamlParser
  , encodeJSON
  , decodeJSON
  , encodeYaml
  , decodeYaml
  , encodeProtoBuf
  , decodeProtoBuf
  , aesonMembers
  , runAesonParse
)   where

import Forestay.Data
import Forestay.Serial.MessagePack as X
import Forestay.Serial.Csv as X

import qualified Data.Aeson as A
import Data.Aeson as X hiding
    ( encode
    , decode
    , (.=)
    , Result
    )
import qualified Data.Aeson.Types as A
import Data.Aeson.Types as X hiding
    ( Options
    , Parser
    , (.=)
    , parse
    , Result
    )

import qualified Data.Aeson.Lens as A
    ( members
    )
import Data.Aeson.Lens as X hiding
    ( members
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
import Control.Lens.Type
    ( IndexedTraversal'
    )

type AesonOptions = A.Options
type AesonParser = A.Parser
type AesonResult = A.Result

type ProtoVal = P.Value
type ProtoField = P.Field

type YamlParser = Y.Parser

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

aesonMembers :: AsValue t => IndexedTraversal' Text t Value
aesonMembers = A.members
{-# INLINE aesonMembers #-}

runAesonParse :: (a -> AesonParser b) -> a -> AesonResult b
runAesonParse = A.parse
{-# INLINE runAesonParse #-}

