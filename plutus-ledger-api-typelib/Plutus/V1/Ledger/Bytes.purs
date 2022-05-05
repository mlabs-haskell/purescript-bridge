-- File auto generated by purescript-bridge! --
module Plutus.V1.Ledger.Bytes where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Lens (Iso', Lens', Prism', iso, prism')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(Nothing, Just))
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Data.Tuple (Tuple(Tuple))
import FromData (class FromData, genericFromData)
import ToData (class ToData, genericToData)
import Type.Proxy (Proxy(Proxy))
import TypeLevel.DataSchema
  ( ApPCons
  , Field
  , I
  , Id
  , IxK
  , MkField
  , MkField_
  , MkIxK
  , MkIxK_
  , PCons
  , PNil
  , PSchema
  , class HasPlutusSchema
  , type (:+)
  , type (:=)
  , type (@@)
  )
import Types.ByteArray (ByteArray)

newtype LedgerBytes = LedgerBytes { getLedgerBytes :: ByteArray }

instance Show LedgerBytes where
  show a = genericShow a

derive instance Generic LedgerBytes _

derive instance Newtype LedgerBytes _

instance
  HasPlutusSchema LedgerBytes
    ( "LedgerBytes"
        :=
          ( "getLedgerBytes" := I ByteArray
              :+ PNil
          )
        @@ (Z)
        :+ PNil
    )

instance ToData LedgerBytes where
  toData x = genericToData x

instance FromData LedgerBytes where
  fromData pd = genericFromData pd

--------------------------------------------------------------------------------

_LedgerBytes :: Iso' LedgerBytes { getLedgerBytes :: ByteArray }
_LedgerBytes = _Newtype
