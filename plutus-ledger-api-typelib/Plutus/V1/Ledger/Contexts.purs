-- File auto generated by purescript-bridge! --
module Plutus.V1.Ledger.Contexts where

import Prelude

import Data.BigInt (BigInt)
import Data.Generic.Rep (class Generic)
import Data.Lens (Iso', Lens', Prism', iso, prism')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(Nothing, Just))
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Data.Tuple (Tuple, Tuple(Tuple))
import FromData (class FromData, genericFromData)
import Plutus.V1.Ledger.Credential (StakingCredential)
import Plutus.V1.Ledger.Crypto (PubKeyHash)
import Plutus.V1.Ledger.DCert (DCert)
import Plutus.V1.Ledger.Interval (Interval)
import Plutus.V1.Ledger.Scripts (Datum, DatumHash)
import Plutus.V1.Ledger.Time (POSIXTime)
import Plutus.V1.Ledger.Tx (TxOut, TxOutRef)
import Plutus.V1.Ledger.TxId (TxId)
import ToData (class ToData, genericToData)
import Type.Proxy (Proxy(Proxy))
import TypeLevel.DataSchema (ApPCons, Field, I, Id, IxK, MkField, MkField_, MkIxK, MkIxK_, PCons, PNil, PSchema, class HasPlutusSchema, type (:+), type (:=), type (@@))
import Types.Value (CurrencySymbol, Value)

newtype TxInfo = TxInfo
  { txInfoInputs :: Array TxInInfo
  , txInfoOutputs :: Array TxOut
  , txInfoFee :: Value
  , txInfoMint :: Value
  , txInfoDCert :: Array DCert
  , txInfoWdrl :: Array (Tuple StakingCredential BigInt)
  , txInfoValidRange :: Interval POSIXTime
  , txInfoSignatories :: Array PubKeyHash
  , txInfoData :: Array (Tuple DatumHash Datum)
  , txInfoId :: TxId
  }

instance Show TxInfo where
  show a = genericShow a

derive instance Generic TxInfo _

derive instance Newtype TxInfo _

instance HasPlutusSchema TxInfo
  ("TxInfo" :=
     ("txInfoInputs" := I (Array TxInInfo)
     :+ "txInfoOutputs" := I (Array TxOut)
     :+ "txInfoFee" := I Value
     :+ "txInfoMint" := I Value
     :+ "txInfoDCert" := I (Array DCert)
     :+ "txInfoWdrl" := I (Array (Tuple StakingCredential BigInt))
     :+ "txInfoValidRange" := I (Interval POSIXTime)
     :+ "txInfoSignatories" := I (Array PubKeyHash)
     :+ "txInfoData" := I (Array (Tuple DatumHash Datum))
     :+ "txInfoId" := I TxId
     :+ PNil)
   @@ (Z)
  :+ PNil)

derive newtype instance ToData TxInfo

derive newtype instance FromData TxInfo

--------------------------------------------------------------------------------

_TxInfo :: Iso' TxInfo {txInfoInputs :: Array TxInInfo, txInfoOutputs :: Array TxOut, txInfoFee :: Value, txInfoMint :: Value, txInfoDCert :: Array DCert, txInfoWdrl :: Array (Tuple StakingCredential BigInt), txInfoValidRange :: Interval POSIXTime, txInfoSignatories :: Array PubKeyHash, txInfoData :: Array (Tuple DatumHash Datum), txInfoId :: TxId}
_TxInfo = _Newtype

--------------------------------------------------------------------------------

newtype TxInInfo = TxInInfo
  { txInInfoOutRef :: TxOutRef
  , txInInfoResolved :: TxOut
  }

instance Show TxInInfo where
  show a = genericShow a

derive instance Generic TxInInfo _

derive instance Newtype TxInInfo _

instance HasPlutusSchema TxInInfo
  ("TxInInfo" :=
     ("txInInfoOutRef" := I TxOutRef
     :+ "txInInfoResolved" := I TxOut
     :+ PNil)
   @@ (Z)
  :+ PNil)

derive newtype instance ToData TxInInfo

derive newtype instance FromData TxInInfo

--------------------------------------------------------------------------------

_TxInInfo :: Iso' TxInInfo {txInInfoOutRef :: TxOutRef, txInInfoResolved :: TxOut}
_TxInInfo = _Newtype

--------------------------------------------------------------------------------

newtype ScriptContext = ScriptContext
  { scriptContextTxInfo :: TxInfo
  , scriptContextPurpose :: ScriptPurpose
  }

instance Show ScriptContext where
  show a = genericShow a

derive instance Generic ScriptContext _

derive instance Newtype ScriptContext _

instance HasPlutusSchema ScriptContext
  ("ScriptContext" :=
     ("scriptContextTxInfo" := I TxInfo
     :+ "scriptContextPurpose" := I ScriptPurpose
     :+ PNil)
   @@ (Z)
  :+ PNil)

derive newtype instance ToData ScriptContext

derive newtype instance FromData ScriptContext

--------------------------------------------------------------------------------

_ScriptContext :: Iso' ScriptContext {scriptContextTxInfo :: TxInfo, scriptContextPurpose :: ScriptPurpose}
_ScriptContext = _Newtype

--------------------------------------------------------------------------------

data ScriptPurpose
  = Minting CurrencySymbol
  | Spending TxOutRef
  | Rewarding StakingCredential
  | Certifying DCert

instance Show ScriptPurpose where
  show a = genericShow a

derive instance Generic ScriptPurpose _

instance HasPlutusSchema ScriptPurpose
  ("Minting" := PNil
   @@ (Z)
  :+ "Spending" := PNil
     @@ (S (Z))
  :+ "Rewarding" := PNil
     @@ (S (S (Z)))
  :+ "Certifying" := PNil
     @@ (S (S (S (Z))))
  :+ PNil)

instance ToData ScriptPurpose where
  toData x = genericToData x

instance FromData ScriptPurpose where
  fromData pd = genericFromData pd

--------------------------------------------------------------------------------

_Minting :: Prism' ScriptPurpose CurrencySymbol
_Minting = prism' Minting case _ of
  (Minting a) -> Just a
  _ -> Nothing

_Spending :: Prism' ScriptPurpose TxOutRef
_Spending = prism' Spending case _ of
  (Spending a) -> Just a
  _ -> Nothing

_Rewarding :: Prism' ScriptPurpose StakingCredential
_Rewarding = prism' Rewarding case _ of
  (Rewarding a) -> Just a
  _ -> Nothing

_Certifying :: Prism' ScriptPurpose DCert
_Certifying = prism' Certifying case _ of
  (Certifying a) -> Just a
  _ -> Nothing
