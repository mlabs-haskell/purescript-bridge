-- File auto generated by purescript-bridge! --
module Data.Tuple where

import Prelude

import ConstrIndices (class HasConstrIndices, constrIndices, fromConstr2Index)
import Data.Generic.Rep (class Generic)
import Data.Lens (Iso', Lens', Prism', iso, prism')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Type.Proxy (Proxy(Proxy))
import Types.Value (CurrencySymbol, TokenName)

newtype Tuple CurrencySymbol TokenName = AssetClass { unAssetClass :: Tuple CurrencySymbol TokenName }

derive instance Generic (Tuple CurrencySymbol TokenName) _

derive instance Newtype (Tuple CurrencySymbol TokenName) _

instance HasConstrIndices (Tuple CurrencySymbol TokenName) where
  constrIndices _ = fromConstr2Index [Tuple "AssetClass" 0]

--------------------------------------------------------------------------------

_AssetClass :: Iso' (Tuple CurrencySymbol TokenName) {unAssetClass :: Tuple CurrencySymbol TokenName}
_AssetClass = _Newtype
