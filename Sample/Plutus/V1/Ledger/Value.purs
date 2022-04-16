-- File auto generated by purescript-bridge! --
module Plutus.V1.Ledger.Value where

import Prelude

import ConstrIndices (class HasConstrIndices, constrIndices, fromConstr2Index)
import Data.BigInt (BigInt)
import Data.Generic.Rep (class Generic)
import Data.Lens (Iso', Lens', Prism', iso, prism')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Tuple (Tuple(..))
import PlutusTx.AssocMap (Map)
import Type.Proxy (Proxy(Proxy))
import Types.ByteArray (ByteArray)

newtype Value = Value { getValue :: Map CurrencySymbol (Map TokenName BigInt) }

derive instance Generic Value _

derive instance Newtype Value _

instance HasConstrIndices Value where
  constrIndices _ = fromConstr2Index [Tuple "Value" 0]

--------------------------------------------------------------------------------

_Value :: Iso' Value {getValue :: Map CurrencySymbol (Map TokenName BigInt)}
_Value = _Newtype

--------------------------------------------------------------------------------

newtype CurrencySymbol = CurrencySymbol { unCurrencySymbol :: ByteArray }

derive instance Generic CurrencySymbol _

derive instance Newtype CurrencySymbol _

instance HasConstrIndices CurrencySymbol where
  constrIndices _ = fromConstr2Index [Tuple "CurrencySymbol" 0]

--------------------------------------------------------------------------------

_CurrencySymbol :: Iso' CurrencySymbol {unCurrencySymbol :: ByteArray}
_CurrencySymbol = _Newtype

--------------------------------------------------------------------------------

newtype AssetClass = AssetClass { unAssetClass :: Tuple CurrencySymbol TokenName }

derive instance Generic AssetClass _

derive instance Newtype AssetClass _

instance HasConstrIndices AssetClass where
  constrIndices _ = fromConstr2Index [Tuple "AssetClass" 0]

--------------------------------------------------------------------------------

_AssetClass :: Iso' AssetClass {unAssetClass :: Tuple CurrencySymbol TokenName}
_AssetClass = _Newtype

--------------------------------------------------------------------------------

newtype TokenName = TokenName { unTokenName :: ByteArray }

derive instance Generic TokenName _

derive instance Newtype TokenName _

instance HasConstrIndices TokenName where
  constrIndices _ = fromConstr2Index [Tuple "TokenName" 0]

--------------------------------------------------------------------------------

_TokenName :: Iso' TokenName {unTokenName :: ByteArray}
_TokenName = _Newtype
