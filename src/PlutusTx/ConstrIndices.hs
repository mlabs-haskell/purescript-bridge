module PlutusTx.ConstrIndices where

import Data.Kind (Type)
import Prelude (Int, String)
import PlutusTx.Aux

import Plutus.V1.Ledger.DCert (DCert(..))
import Plutus.V1.Ledger.Credential (StakingCredential(..), Credential(..))
import Plutus.V1.Ledger.Contexts (ScriptPurpose(..))

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

makeHasConstrIndex ''DCert [('DCertDelegRegKey,0)
                           , ('DCertDelegDeRegKey,1)
                           , ('DCertDelegDelegate,2)
                           , ('DCertPoolRegister,3)
                           , ('DCertPoolRetire,4)
                           , ('DCertGenesis,5)
                           , ('DCertMir,6)
                           ]

makeHasConstrIndex ''StakingCredential [('StakingHash,0), ('StakingPtr,1)]

makeHasConstrIndex ''Credential [('PubKeyCredential,0), ('ScriptCredential,1)]

makeHasConstrIndex ''ScriptPurpose [ ('Minting,0)
                                   , ('Spending,1)
                                   , ('Rewarding,2)
                                   , ('Certifying,3)
                                   ]
