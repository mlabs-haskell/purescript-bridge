-- File auto generated by purescript-bridge! --
module Plutus.V1.Ledger.Value where

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
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))
import FromData (class FromData, genericFromData)
import Plutus.Types.CurrencySymbol (CurrencySymbol)
import Record (get)
import ToData (class ToData, genericToData)
import Type.Proxy (Proxy(Proxy))
import Types.TokenName (TokenName)
import Aeson.Decode as D
import Aeson.Encode as E
import Data.Map as Map

newtype AssetClass = AssetClass (Tuple CurrencySymbol TokenName)

derive instance Eq AssetClass

instance Show AssetClass where
  show a = genericShow a

derive instance Ord AssetClass

derive instance Generic AssetClass _

derive instance Newtype AssetClass _

derive newtype instance ToData AssetClass

derive newtype instance FromData AssetClass

instance EncodeAeson AssetClass where
  encodeAeson' x = pure $ E.encode
    (E.record { unAssetClass: E.value :: _ (Tuple CurrencySymbol TokenName) })
    { unAssetClass: unwrap x }

instance DecodeAeson AssetClass where
  decodeAeson x = wrap <<< get (Proxy :: Proxy "unAssetClass") <$> D.decode
    ( D.record "unAssetClass "
        { unAssetClass: D.value :: _ (Tuple CurrencySymbol TokenName) }
    )
    x

--------------------------------------------------------------------------------

_AssetClass :: Iso' AssetClass (Tuple CurrencySymbol TokenName)
_AssetClass = _Newtype
