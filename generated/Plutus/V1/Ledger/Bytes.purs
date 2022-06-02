-- File auto generated by purescript-bridge! --
module Plutus.V1.Ledger.Bytes where

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
import Record (get)
import ToData (class ToData, genericToData)
import Type.Proxy (Proxy(Proxy))
import Types.ByteArray (ByteArray)
import Aeson as Aeson
import Aeson.Decode as D
import Aeson.Encode as E
import Data.Map as Map

newtype LedgerBytes = LedgerBytes ByteArray

derive instance Eq LedgerBytes

instance Show LedgerBytes where
  show a = genericShow a

instance EncodeAeson LedgerBytes where
  encodeAeson' x = Aeson.encodeAeson' $ E.encode
    (E.record { getLedgerBytes: E.value :: _ (ByteArray) })
    { getLedgerBytes: unwrap x }

instance DecodeAeson LedgerBytes where
  decodeAeson x = wrap <<< get (Proxy :: Proxy "getLedgerBytes") <$> D.decode
    (D.record "getLedgerBytes " { getLedgerBytes: D.value :: _ (ByteArray) })
    x

derive instance Generic LedgerBytes _

derive instance Newtype LedgerBytes _

derive newtype instance ToData LedgerBytes

derive newtype instance FromData LedgerBytes

--------------------------------------------------------------------------------

_LedgerBytes :: Iso' LedgerBytes ByteArray
_LedgerBytes = _Newtype
