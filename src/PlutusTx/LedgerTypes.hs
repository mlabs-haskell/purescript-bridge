{-# LANGUAGE ScopedTypeVariables #-}

module PlutusTx.LedgerTypes where

-- local imports
import PlutusTx.ConstrIndices ()
import Language.PureScript.Bridge
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

-- other imports
import Data.Text

writeLedgerTypes :: FilePath -> IO ()
writeLedgerTypes fp = writeLedgerTypesAnd fp []

writeLedgerTypesAnd :: FilePath -> [SumType 'Haskell] -> IO ()
writeLedgerTypesAnd fp myTypes =
  writePSTypes
    fp
    (buildBridge (defaultBridge <|> plutusBridge))
    (ledgerTypes <> myTypes)

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

-- I'm leaving this commented b/c I'm not sure what the module structure for the ledger types should be.
-- My assumption was that, like Plutarch, we'd just shove everything into it's respective Plutus.V1.Ledger module
plutusBridge :: BridgeBuilder PSType
plutusBridge =
 -- cbtxBridge "Plutus.V1.Ledger.Value" "Value" "Types.Value" "Value"
  --  <|> cbtxBridge "Plutus.V1.Ledger.Value" "CurrencySymbol" "Types.Value" "CurrencySymbol"
  --  <|> cbtxBridge "Plutus.V1.Ledger.Value" "TokenName" "Types.Value" "TokenName"
  --  <|> cbtxBridge "Plutus.V1.Ledger.Address" "Address" "Serialization.Address" "Address"
     cbtxBridge "PlutusTx.Builtins.Internal" "BuiltinByteString" "Types.ByteArray" "ByteArray"
  --  <|> cbtxBridge "Plutus.V1.Ledger.Bytes" "LedgerBytes" "Types.ByteArray" "ByteArray"
  -- <|> cbtxBridge "Plutus.V1.Ledger.Time" "POSIXTime" "Types.Interval" "POSIXTime"
    <|> cbtxBridge "GHC.Integer.Type" "Integer" "Data.BigInt" "BigInt"
    <|> cbtxBridge "PlutusTx.Ratio" "Rational" "Data.Rational" "Rational"
  --  <|> assetClassBridge

cbtxBridge :: Text -> Text -> Text -> Text -> BridgePart
cbtxBridge haskTypeModule haskTypeName psTypeModule psTypeName = do
  typeModule ^== haskTypeModule
  typeName ^== haskTypeName
  return $
    TypeInfo
      { _typePackage = "plutonomicon-cardano-browser-tx"
      , _typeModule = psTypeModule
      , _typeName = psTypeName
      , _typeParameters = []
      }
{-
assetClassBridge :: BridgePart
assetClassBridge = do
  typeModule ^== "Plutus.V1.Ledger.Value"
  typeName ^== "AssetClass"
  return $
    TypeInfo
      { _typePackage = "plutonomicon-cardano-browser-tx"
      , _typeModule = "Data.Tuple"
      , _typeName = "Tuple"
      , _typeParameters = [TypeInfo "" "Types.Value" "CurrencySymbol" [], TypeInfo "" "Types.Value" "TokenName" []]
      }
-}
