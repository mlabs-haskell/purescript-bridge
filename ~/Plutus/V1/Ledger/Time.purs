-- File auto generated by purescript-bridge! --
module Plutus.V1.Ledger.Time where

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
import Type.Proxy (Proxy(Proxy))

newtype DiffMilliSeconds = DiffMilliSeconds BigInt

derive instance Generic DiffMilliSeconds _

derive instance Newtype DiffMilliSeconds _

instance HasConstrIndices DiffMilliSeconds where
  constrIndices _ = fromConstr2Index [Tuple "DiffMilliSeconds" 0]

--------------------------------------------------------------------------------

_DiffMilliSeconds :: Iso' DiffMilliSeconds BigInt
_DiffMilliSeconds = _Newtype
