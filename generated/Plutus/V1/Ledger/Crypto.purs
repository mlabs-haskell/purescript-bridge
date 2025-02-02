-- File auto generated by purescript-bridge! --
module Plutus.V1.Ledger.Crypto where

import Prelude

import Aeson
  ( Aeson
  , aesonNull
  , class DecodeAeson
  , class EncodeAeson
  , decodeAeson
  , encodeAeson
  )
import Aeson.Decode ((</$\>), (</*\>), (</\>), decode, null)
import Aeson.Encode ((>$<), (>/\<), encode, null)
import Control.Lazy (defer)
import Data.Generic.Rep (class Generic)
import Data.Lens (Iso', Lens', Prism', iso, prism')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(Nothing, Just))
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Op (Op(Op))
import Data.Show.Generic (genericShow)
import Data.Tuple.Nested ((/\))
import FromData (class FromData, genericFromData)
import Plutus.V1.Ledger.Bytes (LedgerBytes)
import Record (get)
import ToData (class ToData, genericToData)
import Type.Proxy (Proxy(Proxy))
import Types.ByteArray (ByteArray)
import Aeson as Aeson
import Aeson.Decode as D
import Aeson.Encode as E
import Data.Map as Map

newtype PubKey = PubKey LedgerBytes

derive instance Eq PubKey

instance Show PubKey where
  show a = genericShow a

instance EncodeAeson PubKey where
  encodeAeson' x = Aeson.encodeAeson' $ E.encode
    (E.record { getPubKey: E.value :: _ (LedgerBytes) })
    { getPubKey: unwrap x }

instance DecodeAeson PubKey where
  decodeAeson x = wrap <<< get (Proxy :: Proxy "getPubKey") <$> D.decode
    (D.record "getPubKey " { getPubKey: D.value :: _ (LedgerBytes) })
    x

derive instance Generic PubKey _

derive instance Newtype PubKey _

derive newtype instance ToData PubKey

derive newtype instance FromData PubKey

--------------------------------------------------------------------------------

_PubKey :: Iso' PubKey LedgerBytes
_PubKey = _Newtype

--------------------------------------------------------------------------------

newtype PubKeyHash = PubKeyHash ByteArray

derive instance Eq PubKeyHash

instance Show PubKeyHash where
  show a = genericShow a

instance EncodeAeson PubKeyHash where
  encodeAeson' x = Aeson.encodeAeson' $ E.encode
    (E.record { getPubKeyHash: E.value :: _ (ByteArray) })
    { getPubKeyHash: unwrap x }

instance DecodeAeson PubKeyHash where
  decodeAeson x = wrap <<< get (Proxy :: Proxy "getPubKeyHash") <$> D.decode
    (D.record "getPubKeyHash " { getPubKeyHash: D.value :: _ (ByteArray) })
    x

derive instance Generic PubKeyHash _

derive instance Newtype PubKeyHash _

derive newtype instance ToData PubKeyHash

derive newtype instance FromData PubKeyHash

--------------------------------------------------------------------------------

_PubKeyHash :: Iso' PubKeyHash ByteArray
_PubKeyHash = _Newtype

--------------------------------------------------------------------------------

newtype PrivateKey = PrivateKey LedgerBytes

derive instance Eq PrivateKey

instance Show PrivateKey where
  show a = genericShow a

instance EncodeAeson PrivateKey where
  encodeAeson' x = Aeson.encodeAeson' $ E.encode
    (E.record { getPrivateKey: E.value :: _ (LedgerBytes) })
    { getPrivateKey: unwrap x }

instance DecodeAeson PrivateKey where
  decodeAeson x = wrap <<< get (Proxy :: Proxy "getPrivateKey") <$> D.decode
    (D.record "getPrivateKey " { getPrivateKey: D.value :: _ (LedgerBytes) })
    x

derive instance Generic PrivateKey _

derive instance Newtype PrivateKey _

derive newtype instance ToData PrivateKey

derive newtype instance FromData PrivateKey

--------------------------------------------------------------------------------

_PrivateKey :: Iso' PrivateKey LedgerBytes
_PrivateKey = _Newtype

--------------------------------------------------------------------------------

newtype Signature = Signature ByteArray

derive instance Eq Signature

instance Show Signature where
  show a = genericShow a

instance EncodeAeson Signature where
  encodeAeson' x = Aeson.encodeAeson' $ E.encode
    (E.record { getSignature: E.value :: _ (ByteArray) })
    { getSignature: unwrap x }

instance DecodeAeson Signature where
  decodeAeson x = wrap <<< get (Proxy :: Proxy "getSignature") <$> D.decode
    (D.record "getSignature " { getSignature: D.value :: _ (ByteArray) })
    x

derive instance Generic Signature _

derive instance Newtype Signature _

derive newtype instance ToData Signature

derive newtype instance FromData Signature

--------------------------------------------------------------------------------

_Signature :: Iso' Signature ByteArray
_Signature = _Newtype
