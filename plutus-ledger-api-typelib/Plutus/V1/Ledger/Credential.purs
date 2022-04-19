-- File auto generated by purescript-bridge! --
module Plutus.V1.Ledger.Credential where

import Prelude

import ConstrIndices (class HasConstrIndices, constrIndices, fromConstr2Index)
import Data.BigInt (BigInt)
import Data.Generic.Rep (class Generic)
import Data.Lens (Iso', Lens', Prism', iso, prism')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(Nothing, Just))
import Data.Tuple (Tuple(Tuple))
import FromData (class FromData, fromData, genericFromData)
import Plutus.V1.Ledger.Crypto (PubKeyHash)
import Plutus.V1.Ledger.Scripts (ValidatorHash)
import ToData (class ToData, genericToData, toData)
import Type.Proxy (Proxy(Proxy))

data StakingCredential
  = StakingHash Credential
  | StakingPtr BigInt BigInt BigInt

derive instance Generic StakingCredential _

instance HasConstrIndices StakingCredential where
  constrIndices _ = fromConstr2Index [Tuple "StakingHash" 0,Tuple "StakingPtr" 1]

instance ToData StakingCredential where
  toData x = genericToData x

instance FromData StakingCredential where
  fromData pd = genericFromData pd

--------------------------------------------------------------------------------

_StakingHash :: Prism' StakingCredential Credential
_StakingHash = prism' StakingHash case _ of
  (StakingHash a) -> Just a
  _ -> Nothing

_StakingPtr :: Prism' StakingCredential {a :: BigInt, b :: BigInt, c :: BigInt}
_StakingPtr = prism' (\{a, b, c} -> (StakingPtr a b c)) case _ of
  (StakingPtr a b c) -> Just {a, b, c}
  _ -> Nothing

--------------------------------------------------------------------------------

data Credential
  = PubKeyCredential PubKeyHash
  | ScriptCredential ValidatorHash

derive instance Generic Credential _

instance HasConstrIndices Credential where
  constrIndices _ = fromConstr2Index [Tuple "PubKeyCredential" 0,Tuple "ScriptCredential" 1]

instance ToData Credential where
  toData x = genericToData x

instance FromData Credential where
  fromData pd = genericFromData pd

--------------------------------------------------------------------------------

_PubKeyCredential :: Prism' Credential PubKeyHash
_PubKeyCredential = prism' PubKeyCredential case _ of
  (PubKeyCredential a) -> Just a
  _ -> Nothing

_ScriptCredential :: Prism' Credential ValidatorHash
_ScriptCredential = prism' ScriptCredential case _ of
  (ScriptCredential a) -> Just a
  _ -> Nothing
