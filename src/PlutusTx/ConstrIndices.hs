{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module PlutusTx.ConstrIndices (HasConstrIndices (getConstrIndices)) where

import Data.Kind (Type)
import PlutusTx.Aux (makeHasConstrIndex)
import Prelude (Int, String)

import Plutus.V1.Ledger.Api (DCert (DCertDelegRegKey))
import Plutus.V1.Ledger.Contexts (ScriptPurpose (Certifying, Minting, Rewarding, Spending))
import Plutus.V1.Ledger.Credential (Credential (PubKeyCredential, ScriptCredential), StakingCredential (StakingHash, StakingPtr))
import Plutus.V1.Ledger.DCert (
  DCert (DCertDelegDeRegKey, DCertDelegDelegate, DCertGenesis, DCertMir, DCertPoolRegister, DCertPoolRetire),
 )
import Plutus.V1.Ledger.Interval (Extended (Finite, NegInf, PosInf))

-- This module contains the HasConstrIndices class, which is used for generating the corresponding PureScript class of
-- the same name. It also contains instances for Plutus Ledger types with multiple constructors because we cannot hook
-- @makeIsDataIndexed@ in IOHK's Plutus modules

-- The index information for the ledger types is copy/pasted from the Plutus source.

-- TODO: In theory we could avoid copy/pasting and use compile-time IO with template Haskell to query/parse the index information
--       from a particular commit on the Plutus gihub repo. That would be annoying to implement but would provide a high
--       degree of assurance that the index information will always be current relative to a specific commit in their repo.
--       (I don't want to do this.)

class HasConstrIndices (a :: Type) where
  getConstrIndices :: [(Int, String)]

makeHasConstrIndex
  ''DCert
  [ ('DCertDelegRegKey, 0)
  , ('DCertDelegDeRegKey, 1)
  , ('DCertDelegDelegate, 2)
  , ('DCertPoolRegister, 3)
  , ('DCertPoolRetire, 4)
  , ('DCertGenesis, 5)
  , ('DCertMir, 6)
  ]

makeHasConstrIndex ''StakingCredential [('StakingHash, 0), ('StakingPtr, 1)]

makeHasConstrIndex ''Credential [('PubKeyCredential, 0), ('ScriptCredential, 1)]

makeHasConstrIndex
  ''ScriptPurpose
  [ ('Minting, 0)
  , ('Spending, 1)
  , ('Rewarding, 2)
  , ('Certifying, 3)
  ]

makeHasConstrIndex ''Extended [('NegInf, 0), ('Finite, 1), ('PosInf, 2)]
