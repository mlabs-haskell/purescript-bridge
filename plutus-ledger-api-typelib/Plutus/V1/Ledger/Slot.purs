-- File auto generated by purescript-bridge! --
module Plutus.V1.Ledger.Slot where

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
import TypeLevel.DataSchema (ApPCons, Field, I, Id, IxK, MkField, MkField_, MkIxK, MkIxK_, PCons, PNil, PSchema, class HasPlutusSchema, type (:+), type (:=), type (@@))

newtype Slot = Slot { getSlot :: BigInt }

instance Show Slot where
  show a = genericShow a

derive instance Eq Slot

derive instance Ord Slot

derive instance Generic Slot _

derive instance Newtype Slot _

instance HasPlutusSchema Slot
  ("Slot" :=
     ("getSlot" := I BigInt
     :+ PNil)
   @@ (Z)
  :+ PNil)

instance ToData Slot where
  toData x = genericToData x

instance FromData Slot where
  fromData pd = genericFromData pd

--------------------------------------------------------------------------------

_Slot :: Iso' Slot {getSlot :: BigInt}
_Slot = _Newtype
