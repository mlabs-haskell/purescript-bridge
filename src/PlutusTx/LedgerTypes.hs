{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module PlutusTx.LedgerTypes where

-- local imports

-- local imports
import Language.PureScript.Bridge (
  BridgeBuilder,
  BridgePart,
  Language (Haskell),
  PSType,
  SumType,
  TypeInfo (
    TypeInfo,
    _typeModule,
    _typeName,
    _typePackage,
    _typeParameters
  ),
  buildBridge,
  defaultBridge,
  extremelyUnsafeMkSumType,
  mkSumTypeIndexed,
  order,
  psTypeParameters,
  typeModule,
  typeName,
  writePSTypes,
  (<|>),
  (^==),
 )
import Language.PureScript.Bridge.TypeParameters (A)
import PlutusTx.ConstrIndices ()

-- Ledger type imports
import Plutus.V1.Ledger.Ada (Ada)

-- import Plutus.V1.Ledger.Address (Address)
import Plutus.V1.Ledger.Bytes (LedgerBytes)
import Plutus.V1.Ledger.Contexts (ScriptContext, ScriptPurpose, TxInInfo, TxInfo)
import Plutus.V1.Ledger.Credential (Credential, StakingCredential)
import Plutus.V1.Ledger.Crypto (PrivateKey, PubKey, PubKeyHash, Signature)
import Plutus.V1.Ledger.DCert (DCert)
import Plutus.V1.Ledger.Interval (Extended, Interval, LowerBound, UpperBound)
import Plutus.V1.Ledger.Scripts (
  Datum,
  DatumHash,
  MintingPolicyHash,
  Redeemer,
  ScriptHash,
  StakeValidatorHash,
  ValidatorHash,
 )
import Plutus.V1.Ledger.Slot (Slot)
import Plutus.V1.Ledger.Time (DiffMilliSeconds, POSIXTime)
import Plutus.V1.Ledger.Tx (TxOut, TxOutRef)
import Plutus.V1.Ledger.TxId (TxId)
import Plutus.V1.Ledger.Value (AssetClass)

-- import Plutus.V1.Ledger.Value (AssetClass, CurrencySymbol, TokenName, Value)

import Data.Text (Text)

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
  -- [ extremelyUnsafeMkSumType @Value
  -- , order $ extremelyUnsafeMkSumType @CurrencySymbol
  -- , order $ extremelyUnsafeMkSumType @TokenName
  [ order $ extremelyUnsafeMkSumType @AssetClass
  , order $ extremelyUnsafeMkSumType @TxId
  , extremelyUnsafeMkSumType @TxOut
  , extremelyUnsafeMkSumType @TxOutRef
  , order $ extremelyUnsafeMkSumType @DiffMilliSeconds
  , order $ extremelyUnsafeMkSumType @POSIXTime
  , order $ extremelyUnsafeMkSumType @Slot
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
  , --  , extremelyUnsafeMkSumType @Address
    extremelyUnsafeMkSumType @Ada
  , extremelyUnsafeMkSumType @(Interval A)
  , extremelyUnsafeMkSumType @(LowerBound A)
  , extremelyUnsafeMkSumType @(UpperBound A)
  , mkSumTypeIndexed @DCert
  , mkSumTypeIndexed @(Extended A)
  , mkSumTypeIndexed @StakingCredential
  , mkSumTypeIndexed @Credential
  , mkSumTypeIndexed @ScriptPurpose
  ]

plutusBridge :: BridgeBuilder PSType
plutusBridge =
  -- Primitive types
  cbtxBridge "PlutusTx.Builtins.Internal" "BuiltinByteString" "Types.ByteArray" "ByteArray"
    <|> cbtxBridge "PlutusTx.Builtins.Internal" "BuiltinData" "Types.PlutusData" "PlutusData"
    <|> cbtxBridge "GHC.Integer.Type" "Integer" "Data.BigInt" "BigInt"
    <|> cbtxBridge "PlutusTx.Ratio" "Rational" "Types.Rational" "Rational"
    <|> mapBridge
    -- ledger-api types
    <|> cbtxBridge "Plutus.V1.Ledger.Value" "Value" "Types.Value" "Value"
    <|> cbtxBridge "Plutus.V1.Ledger.Value" "CurrencySymbol" "Types.Value" "CurrencySymbol"
    <|> cbtxBridge "Plutus.V1.Ledger.Value" "TokenName" "Types.Value" "TokenName"
    <|> cbtxBridge "Plutus.V1.Ledger.Address" "Address" "Plutus.Types.Address" "Address"

cbtxBridge :: Text -> Text -> Text -> Text -> BridgePart
cbtxBridge haskTypeModule haskTypeName psTypeModule psTypeName = do
  typeModule ^== haskTypeModule
  typeName ^== haskTypeName
  return $
    TypeInfo
      { _typePackage = "plutonomicon-cardano-transaction-lib"
      , _typeModule = psTypeModule
      , _typeName = psTypeName
      , _typeParameters = []
      }

mapBridge :: BridgePart
mapBridge = do
  typeModule ^== "PlutusTx.AssocMap"
  typeName ^== "Map"
  TypeInfo "plutonomicon-cardano-transaction-lib" "Plutus.Types.AssocMap" "Map" <$> psTypeParameters
