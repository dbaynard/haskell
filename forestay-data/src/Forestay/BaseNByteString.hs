{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, ApplicativeDo, PartialTypeSignatures #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable, DeriveGeneric #-}

module Forestay.BaseNByteString (
    Base16()
  , Base64()
  , Base64URL()
  , LBase16()
  , LBase64()
  , LBase64URL()
  , B6.joinWith
  , encodeBase16
  , decodeBase16
  , encodeLBase16
  , decodeLBase16
  , encodeBase64
  , decodeBase64
  , decodeLenientBase64
  , encodeLBase64
  , decodeLBase64
  , decodeLenientLBase64
  , encodeBase64URL
  , decodeBase64URL
  , decodeLenientBase64URL
  , encodeLBase64URL
  , decodeLBase64URL
  , decodeLenientLBase64URL
)   where

import Protolude.Lifted
import Data.Data

import Data.String (String)

import qualified Data.ByteString.Base16 as B4
import qualified Data.ByteString.Base64 as B6
import qualified Data.ByteString.Base64.URL as BU

import qualified Data.ByteString.Base16.Lazy as B4L
import qualified Data.ByteString.Base64.Lazy as B6L
import qualified Data.ByteString.Base64.URL.Lazy as BUL

newtype Base16 = MakeBase16 { _base16 :: ByteString }
        deriving (Eq, Ord, Read, Show, Generic, Data, Typeable, NFData, IsString, Monoid, Semigroup, Print)

newtype Base64 = MakeBase64 { _base64 :: ByteString }
        deriving (Eq, Ord, Read, Show, Generic, Data, Typeable, NFData, IsString, Monoid, Semigroup, Print)

newtype Base64URL = MakeBase64URL { _base64URL :: ByteString }
        deriving (Eq, Ord, Read, Show, Generic, Data, Typeable, NFData, IsString, Monoid, Semigroup, Print)

newtype LBase16 = MakeLBase16 { _lBase16 :: LByteString }
        deriving (Eq, Ord, Read, Show, Generic, Data, Typeable, NFData, IsString, Monoid, Semigroup, Print)

newtype LBase64 = MakeLBase64 { _lBase64 :: LByteString }
        deriving (Eq, Ord, Read, Show, Generic, Data, Typeable, NFData, IsString, Monoid, Semigroup, Print)

newtype LBase64URL = MakeLBase64URL { _lBase64URL :: LByteString }
        deriving (Eq, Ord, Read, Show, Generic, Data, Typeable, NFData, IsString, Monoid, Semigroup, Print)

encodeBase16 :: ByteString -> Base16
encodeBase16 = MakeBase16 . B4.encode
{-# INLINE encodeBase16 #-}

decodeBase16 :: Base16 -> (ByteString, ByteString)
decodeBase16 = B4.decode . _base16
{-# INLINE decodeBase16 #-}

encodeLBase16 :: LByteString -> LBase16
encodeLBase16 = MakeLBase16 . B4L.encode
{-# INLINE encodeLBase16 #-}

decodeLBase16 :: LBase16 -> (LByteString, LByteString)
decodeLBase16 = B4L.decode . _lBase16
{-# INLINE decodeLBase16 #-}

encodeBase64 :: ByteString -> Base64
encodeBase64 = MakeBase64 . B6.encode
{-# INLINE encodeBase64 #-}

decodeBase64 :: Base64 -> Either String ByteString
decodeBase64 = B6.decode . _base64
{-# INLINE decodeBase64 #-}

decodeLenientBase64 :: Base64 -> ByteString
decodeLenientBase64 = B6.decodeLenient . _base64
{-# INLINE decodeLenientBase64 #-}

encodeLBase64 :: LByteString -> LBase64
encodeLBase64 = MakeLBase64 . B6L.encode
{-# INLINE encodeLBase64 #-}

decodeLBase64 :: LBase64 -> Either String LByteString
decodeLBase64 = B6L.decode . _lBase64
{-# INLINE decodeLBase64 #-}

decodeLenientLBase64 :: LBase64 -> LByteString
decodeLenientLBase64 = B6L.decodeLenient . _lBase64
{-# INLINE decodeLenientLBase64 #-}

encodeBase64URL :: ByteString -> Base64URL
encodeBase64URL = MakeBase64URL . BU.encode
{-# INLINE encodeBase64URL #-}

decodeBase64URL :: Base64URL -> Either String ByteString
decodeBase64URL = BU.decode . _base64URL
{-# INLINE decodeBase64URL #-}

decodeLenientBase64URL :: Base64URL -> ByteString
decodeLenientBase64URL = BU.decodeLenient . _base64URL
{-# INLINE decodeLenientBase64URL #-}

encodeLBase64URL :: LByteString -> LBase64URL
encodeLBase64URL = MakeLBase64URL . BUL.encode
{-# INLINE encodeLBase64URL #-}

decodeLBase64URL :: LBase64URL -> Either String LByteString
decodeLBase64URL = BUL.decode . _lBase64URL
{-# INLINE decodeLBase64URL #-}

decodeLenientLBase64URL :: LBase64URL -> LByteString
decodeLenientLBase64URL = BUL.decodeLenient . _lBase64URL
{-# INLINE decodeLenientLBase64URL #-}

