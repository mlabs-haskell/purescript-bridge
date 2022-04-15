{-# LANGUAGE ScopedTypeVariables #-}

module PlutusTx.LedgerTypes where

-- local imports
import PlutusTx.ConstrIndices ()
import Language.PureScript.Bridge.SumType
import Language.PureScript.Bridge.TypeInfo
import Language.PureScript.Bridge.TypeParameters (A)

-- Ledger type imports
import Plutus.V1.Ledger.Ada (Ada)
import Plutus.V1.Ledger.Address (Address)
import Plutus.V1.Ledger.Bytes (LedgerBytes)
import Plutus.V1.Ledger.Contexts (ScriptContext,ScriptPurpose,TxInfo,TxInInfo)
import Plutus.V1.Ledger.Credential (StakingCredential, Credential)
import Plutus.V1.Ledger.Crypto (PubKey,PubKeyHash,PrivateKey,Signature)
import Plutus.V1.Ledger.DCert (DCert)
import Plutus.V1.Ledger.Interval (Interval, UpperBound, LowerBound, Extended)
import Plutus.V1.Ledger.Value (CurrencySymbol, AssetClass, Value, TokenName)
import Plutus.V1.Ledger.TxId (TxId)
import Plutus.V1.Ledger.Tx (TxOut,TxOutRef)
import Plutus.V1.Ledger.Time (POSIXTime,DiffMilliSeconds)
import Plutus.V1.Ledger.Slot (Slot)
import Plutus.V1.Ledger.Scripts (
  Redeemer,
  Datum,
  ScriptHash,
  ValidatorHash,
  DatumHash,
  MintingPolicyHash,
  StakeValidatorHash,
 )


ledgerTypes :: [SumType 'Haskell]
ledgerTypes =
  [ -- Newtypes or single constructor data types (extremelyUnsafeMkSumType is safe)
    extremelyUnsafeMkSumType @Value
  , extremelyUnsafeMkSumType @CurrencySymbol
  , extremelyUnsafeMkSumType @AssetClass
  , extremelyUnsafeMkSumType @TokenName
  , extremelyUnsafeMkSumType @TxId
  , extremelyUnsafeMkSumType @TxOut
  , extremelyUnsafeMkSumType @TxOutRef
  , extremelyUnsafeMkSumType @DiffMilliSeconds
  , extremelyUnsafeMkSumType @POSIXTime
  , extremelyUnsafeMkSumType @Slot
  , extremelyUnsafeMkSumType @Redeemer
  , extremelyUnsafeMkSumType @Datum
  , extremelyUnsafeMkSumType @ScriptHash
  , extremelyUnsafeMkSumType @ValidatorHash
  , extremelyUnsafeMkSumType @DatumHash
  , extremelyUnsafeMkSumType @MintingPolicyHash
  , extremelyUnsafeMkSumType @StakeValidatorHash
  , extremelyUnsafeMkSumType @PubKey
  , extremelyUnsafeMkSumType @PubKeyHash
  , extremelyUnsafeMkSumType @PrivateKey
  , extremelyUnsafeMkSumType @Signature
  , extremelyUnsafeMkSumType @TxInfo
  , extremelyUnsafeMkSumType @TxInInfo
  , extremelyUnsafeMkSumType @ScriptContext
  , extremelyUnsafeMkSumType @LedgerBytes
  , extremelyUnsafeMkSumType @Address
  , extremelyUnsafeMkSumType @Ada
  , extremelyUnsafeMkSumType @(Interval A)
  , extremelyUnsafeMkSumType @(LowerBound A)
  , extremelyUnsafeMkSumType @(UpperBound A)

  -- True sum types, HasConstrIndices instances generated in the PlutusTx.ConstrIndices module
  , mkSumTypeIndexed @DCert
  , mkSumTypeIndexed @(Extended A)
  , mkSumTypeIndexed @StakingCredential
  , mkSumTypeIndexed @Credential
  , mkSumTypeIndexed @ScriptPurpose
  ]
