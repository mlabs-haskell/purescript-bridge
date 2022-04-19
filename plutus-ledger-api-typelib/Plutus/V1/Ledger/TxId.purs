-- File auto generated by purescript-bridge! --
module Plutus.V1.Ledger.TxId where

import Prelude

import ConstrIndices (class HasConstrIndices, constrIndices, fromConstr2Index)
import Data.Generic.Rep (class Generic)
import Data.Lens (Iso', Lens', Prism', iso, prism')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(Nothing, Just))
import Data.Newtype (class Newtype)
import Data.Tuple (Tuple(Tuple))
import FromData (class FromData, fromData, genericFromData)
import ToData (class ToData, genericToData, toData)
import Type.Proxy (Proxy(Proxy))
import Types.ByteArray (ByteArray)

newtype TxId = TxId { getTxId :: ByteArray }

derive instance Generic TxId _

derive instance Newtype TxId _

instance HasConstrIndices TxId where
  constrIndices _ = fromConstr2Index [Tuple "TxId" 0]

instance ToData TxId where
  toData x = genericToData x

instance FromData TxId where
  fromData pd = genericFromData pd

--------------------------------------------------------------------------------

_TxId :: Iso' TxId {getTxId :: ByteArray}
_TxId = _Newtype