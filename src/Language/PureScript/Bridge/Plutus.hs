{-# LANGUAGE AllowAmbiguousTypes #-}

module Language.PureScript.Bridge.Plutus (
  HasConstrIndices (..),
  mkSumTypeIndexed,
) where

import Data.Kind (Type)
import Data.Typeable (Typeable)
import GHC.Generics (Generic (Rep))

import Language.PureScript.Bridge.SumType (GDataConstructor, SumType)
import Language.PureScript.Bridge.SumType qualified as PB
import Language.PureScript.Bridge.TypeInfo (Language (Haskell))
import PlutusTx.Aux (HasConstrIndices (getConstrIndices))

mkSumTypeIndexed ::
  forall (t :: Type).
  ( Generic t
  , Typeable t
  , GDataConstructor (Rep t)
  , HasConstrIndices t
  ) =>
  SumType 'Haskell
mkSumTypeIndexed = PB.mkSumTypeIndexed @HasConstrIndices @t (getConstrIndices @t)
