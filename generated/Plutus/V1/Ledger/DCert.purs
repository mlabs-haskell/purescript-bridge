-- File auto generated by purescript-bridge! --
module Plutus.V1.Ledger.DCert where

import Prelude

import Aeson (Aeson, aesonNull, class DecodeAeson, class EncodeAeson, decodeAeson, encodeAeson)
import Aeson.Decode ((</$\>), (</*\>), (</\>), decode, null)
import Aeson.Encode ((>$<), (>/\<), encode, null)
import Control.Lazy (defer)
import Data.BigInt (BigInt)
import Data.Generic.Rep (class Generic)
import Data.Lens (Iso', Lens', Prism', iso, prism')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(Nothing, Just))
import Data.Newtype (unwrap, wrap)
import Data.Op (Op(Op))
import Data.Show.Generic (genericShow)
import Data.Tuple (Tuple(Tuple))
import Data.Tuple.Nested ((/\))
import FromData (class FromData, genericFromData)
import Plutus.Types.DataSchema (ApPCons, Field, I, Id, IxK, MkField, MkField_, MkIxK, MkIxK_, PCons, PNil, PSchema, class HasPlutusSchema, type (:+), type (:=), type (@@))
import Plutus.V1.Ledger.Credential (StakingCredential)
import Plutus.V1.Ledger.Crypto (PubKeyHash)
import ToData (class ToData, genericToData)
import Type.Proxy (Proxy(Proxy))
import TypeLevel.Nat (S, Z)
import Aeson as Aeson
import Aeson.Decode as D
import Aeson.Encode as E
import Data.Map as Map

data DCert
  = DCertDelegRegKey StakingCredential
  | DCertDelegDeRegKey StakingCredential
  | DCertDelegDelegate StakingCredential PubKeyHash
  | DCertPoolRegister PubKeyHash PubKeyHash
  | DCertPoolRetire PubKeyHash BigInt
  | DCertGenesis
  | DCertMir

derive instance Eq DCert

instance Show DCert where
  show a = genericShow a

instance EncodeAeson DCert where
  encodeAeson' x = Aeson.encodeAeson' $ (defer \_ ->  case _ of
    DCertDelegRegKey a -> E.encodeTagged "DCertDelegRegKey" a E.value
    DCertDelegDeRegKey a -> E.encodeTagged "DCertDelegDeRegKey" a E.value
    DCertDelegDelegate a b -> E.encodeTagged "DCertDelegDelegate" (a /\ b) (E.tuple (E.value >/\< E.value))
    DCertPoolRegister a b -> E.encodeTagged "DCertPoolRegister" (a /\ b) (E.tuple (E.value >/\< E.value))
    DCertPoolRetire a b -> E.encodeTagged "DCertPoolRetire" (a /\ b) (E.tuple (E.value >/\< E.value))
    DCertGenesis -> encodeAeson { tag: "DCertGenesis" }
    DCertMir -> encodeAeson { tag: "DCertMir" } ) x

instance DecodeAeson DCert where
  decodeAeson = defer \_ -> D.decode
    $ D.sumType "DCert" $ Map.fromFoldable
      [ "DCertDelegRegKey" /\ D.content (DCertDelegRegKey <$> D.value)
      , "DCertDelegDeRegKey" /\ D.content (DCertDelegDeRegKey <$> D.value)
      , "DCertDelegDelegate" /\ D.content (D.tuple $ DCertDelegDelegate </$\>D.value </*\> D.value)
      , "DCertPoolRegister" /\ D.content (D.tuple $ DCertPoolRegister </$\>D.value </*\> D.value)
      , "DCertPoolRetire" /\ D.content (D.tuple $ DCertPoolRetire </$\>D.value </*\> D.value)
      , "DCertGenesis" /\ pure DCertGenesis
      , "DCertMir" /\ pure DCertMir
      ]

derive instance Generic DCert _

instance HasPlutusSchema DCert
  ("DCertDelegRegKey" := PNil
   @@ (Z)
  :+ "DCertDelegDeRegKey" := PNil
     @@ (S (Z))
  :+ "DCertDelegDelegate" := PNil
     @@ (S (S (Z)))
  :+ "DCertPoolRegister" := PNil
     @@ (S (S (S (Z))))
  :+ "DCertPoolRetire" := PNil
     @@ (S (S (S (S (Z)))))
  :+ "DCertGenesis" := PNil
     @@ (S (S (S (S (S (Z))))))
  :+ "DCertMir" := PNil
     @@ (S (S (S (S (S (S (Z)))))))
  :+ PNil)

instance ToData DCert where
  toData x = genericToData x

instance FromData DCert where
  fromData x = genericFromData x

--------------------------------------------------------------------------------

_DCertDelegRegKey :: Prism' DCert StakingCredential
_DCertDelegRegKey = prism' DCertDelegRegKey case _ of
  (DCertDelegRegKey a) -> Just a
  _ -> Nothing

_DCertDelegDeRegKey :: Prism' DCert StakingCredential
_DCertDelegDeRegKey = prism' DCertDelegDeRegKey case _ of
  (DCertDelegDeRegKey a) -> Just a
  _ -> Nothing

_DCertDelegDelegate :: Prism' DCert {a :: StakingCredential, b :: PubKeyHash}
_DCertDelegDelegate = prism' (\{a, b} -> (DCertDelegDelegate a b)) case _ of
  (DCertDelegDelegate a b) -> Just {a, b}
  _ -> Nothing

_DCertPoolRegister :: Prism' DCert {a :: PubKeyHash, b :: PubKeyHash}
_DCertPoolRegister = prism' (\{a, b} -> (DCertPoolRegister a b)) case _ of
  (DCertPoolRegister a b) -> Just {a, b}
  _ -> Nothing

_DCertPoolRetire :: Prism' DCert {a :: PubKeyHash, b :: BigInt}
_DCertPoolRetire = prism' (\{a, b} -> (DCertPoolRetire a b)) case _ of
  (DCertPoolRetire a b) -> Just {a, b}
  _ -> Nothing

_DCertGenesis :: Prism' DCert Unit
_DCertGenesis = prism' (const DCertGenesis) case _ of
  DCertGenesis -> Just unit
  _ -> Nothing

_DCertMir :: Prism' DCert Unit
_DCertMir = prism' (const DCertMir) case _ of
  DCertMir -> Just unit
  _ -> Nothing