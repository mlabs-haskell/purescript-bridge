-- File auto generated by purescript-bridge! --
module Plutus.V1.Ledger.Time where

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
import Data.BigInt (BigInt)
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
import ToData (class ToData, genericToData)
import Type.Proxy (Proxy(Proxy))
import Aeson as Aeson
import Aeson.Decode as D
import Aeson.Encode as E
import Data.Map as Map

newtype DiffMilliSeconds = DiffMilliSeconds BigInt

derive instance Eq DiffMilliSeconds

instance Show DiffMilliSeconds where
  show a = genericShow a

instance EncodeAeson DiffMilliSeconds where
  encodeAeson' x = Aeson.encodeAeson' $
    (defer \_ -> E.encode $ unwrap >$< E.value) x

instance DecodeAeson DiffMilliSeconds where
  decodeAeson = defer \_ -> D.decode $ (DiffMilliSeconds <$> D.value)

derive instance Ord DiffMilliSeconds

derive instance Generic DiffMilliSeconds _

derive instance Newtype DiffMilliSeconds _

derive newtype instance ToData DiffMilliSeconds

derive newtype instance FromData DiffMilliSeconds

--------------------------------------------------------------------------------

_DiffMilliSeconds :: Iso' DiffMilliSeconds BigInt
_DiffMilliSeconds = _Newtype

--------------------------------------------------------------------------------

newtype POSIXTime = POSIXTime BigInt

derive instance Eq POSIXTime

instance Show POSIXTime where
  show a = genericShow a

instance EncodeAeson POSIXTime where
  encodeAeson' x = Aeson.encodeAeson' $
    (defer \_ -> E.encode $ unwrap >$< E.value) x

instance DecodeAeson POSIXTime where
  decodeAeson = defer \_ -> D.decode $ (POSIXTime <$> D.value)

derive instance Ord POSIXTime

derive instance Generic POSIXTime _

derive instance Newtype POSIXTime _

derive newtype instance ToData POSIXTime

derive newtype instance FromData POSIXTime

--------------------------------------------------------------------------------

_POSIXTime :: Iso' POSIXTime BigInt
_POSIXTime = _Newtype
