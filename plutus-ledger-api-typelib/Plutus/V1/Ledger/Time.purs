-- File auto generated by purescript-bridge! --
module Plutus.V1.Ledger.Time where

import Prelude

import Data.BigInt (BigInt)
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

newtype DiffMilliSeconds = DiffMilliSeconds BigInt

instance Show DiffMilliSeconds where
  show a = genericShow a

derive instance Eq DiffMilliSeconds

derive instance Ord DiffMilliSeconds

derive instance Generic DiffMilliSeconds _

derive instance Newtype DiffMilliSeconds _

instance
  HasPlutusSchema DiffMilliSeconds
    ( "DiffMilliSeconds" := PNil
        @@ (Z)
        :+ PNil
    )

instance ToData DiffMilliSeconds where
  toData x = genericToData x

instance FromData DiffMilliSeconds where
  fromData pd = genericFromData pd

--------------------------------------------------------------------------------

_DiffMilliSeconds :: Iso' DiffMilliSeconds BigInt
_DiffMilliSeconds = _Newtype

--------------------------------------------------------------------------------

newtype POSIXTime = POSIXTime { getPOSIXTime :: BigInt }

instance Show POSIXTime where
  show a = genericShow a

derive instance Eq POSIXTime

derive instance Ord POSIXTime

derive instance Generic POSIXTime _

derive instance Newtype POSIXTime _

instance
  HasPlutusSchema POSIXTime
    ( "POSIXTime"
        :=
          ( "getPOSIXTime" := I BigInt
              :+ PNil
          )
        @@ (Z)
        :+ PNil
    )

instance ToData POSIXTime where
  toData x = genericToData x

instance FromData POSIXTime where
  fromData pd = genericFromData pd

--------------------------------------------------------------------------------

_POSIXTime :: Iso' POSIXTime { getPOSIXTime :: BigInt }
_POSIXTime = _Newtype
