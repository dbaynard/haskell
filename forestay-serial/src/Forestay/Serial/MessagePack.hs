{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, ApplicativeDo, PartialTypeSignatures, TypeApplications #-}

module Forestay.Serial.MessagePack (
    module X
  , MPObject
  , putTextStr
)   where

import Forestay.Data

import qualified Data.MessagePack as M
import Data.MessagePack as X hiding
    ( Object()
    , putStr
    )

import qualified Data.Binary.Put as B (Put)

type MPObject = M.Object

putTextStr :: Text -> B.Put
putTextStr = M.putStr
{-# INLINE putTextStr #-}

