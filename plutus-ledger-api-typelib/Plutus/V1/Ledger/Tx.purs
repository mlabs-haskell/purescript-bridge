-- File auto generated by purescript-bridge! --
module Plutus.V1.Ledger.Tx where

import Prelude

import Data.BigInt (BigInt)
import Data.Generic.Rep (class Generic)
import Data.Lens (Iso', Lens', Prism', iso, prism')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe, Maybe(Nothing, Just))
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Data.Tuple (Tuple(Tuple))
import FromData (class FromData, genericFromData)
import Plutus.Types.Address (Address)
import Plutus.V1.Ledger.Scripts (DatumHash)
import Plutus.V1.Ledger.TxId (TxId)
import ToData (class ToData, genericToData)
import Type.Proxy (Proxy(Proxy))
import TypeLevel.DataSchema (ApPCons, Field, I, Id, IxK, MkField, MkField_, MkIxK, MkIxK_, PCons, PNil, PSchema, class HasPlutusSchema, type (:+), type (:=), type (@@))
import Types.Value (Value)

newtype TxOut = TxOut
  { txOutAddress :: Address
  , txOutValue :: Value
  , txOutDatumHash :: Maybe DatumHash
  }

instance Show TxOut where
  show a = genericShow a

derive instance Generic TxOut _

derive instance Newtype TxOut _

instance HasPlutusSchema TxOut
  ("TxOut" :=
     ("txOutAddress" := I Address
     :+ "txOutValue" := I Value
     :+ "txOutDatumHash" := I (Maybe DatumHash)
     :+ PNil)
   @@ (Z)
  :+ PNil)

derive newtype instance ToData TxOut

derive newtype instance FromData TxOut

--------------------------------------------------------------------------------

_TxOut :: Iso' TxOut {txOutAddress :: Address, txOutValue :: Value, txOutDatumHash :: Maybe DatumHash}
_TxOut = _Newtype

--------------------------------------------------------------------------------

newtype TxOutRef = TxOutRef
  { txOutRefId :: TxId
  , txOutRefIdx :: BigInt
  }

instance Show TxOutRef where
  show a = genericShow a

derive instance Generic TxOutRef _

derive instance Newtype TxOutRef _

instance HasPlutusSchema TxOutRef
  ("TxOutRef" :=
     ("txOutRefId" := I TxId
     :+ "txOutRefIdx" := I BigInt
     :+ PNil)
   @@ (Z)
  :+ PNil)

derive newtype instance ToData TxOutRef

derive newtype instance FromData TxOutRef

--------------------------------------------------------------------------------

_TxOutRef :: Iso' TxOutRef {txOutRefId :: TxId, txOutRefIdx :: BigInt}
_TxOutRef = _Newtype
