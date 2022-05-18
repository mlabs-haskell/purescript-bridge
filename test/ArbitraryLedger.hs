module ArbitraryLedger where

-- Ledger type imports
import Plutus.V1.Ledger.Ada (Ada(..))
import Plutus.V1.Ledger.Address (Address(..))
import Plutus.V1.Ledger.Bytes (LedgerBytes(..))
import Plutus.V1.Ledger.Contexts (ScriptContext(..), ScriptPurpose(..), TxInInfo(..), TxInfo(..))
import Plutus.V1.Ledger.Credential (Credential(..), StakingCredential(..))
import Plutus.V1.Ledger.Crypto (PrivateKey(..), PubKey(..), PubKeyHash(..), Signature(..))
import Plutus.V1.Ledger.DCert (DCert(..))
import Plutus.V1.Ledger.Interval (Extended(..), Interval(..), LowerBound(..), UpperBound(..))
import Plutus.V1.Ledger.Scripts (
  Datum(..),
  DatumHash(..),
  MintingPolicy(..),
  MintingPolicyHash(..),
  Redeemer(..),
  ScriptHash(..),
  StakeValidatorHash(..),
  Validator(..),
  ValidatorHash(..),
 )
import Plutus.V1.Ledger.Slot (Slot(..))
import Plutus.V1.Ledger.Time (DiffMilliSeconds(..), POSIXTime(..))
import Plutus.V1.Ledger.Tx (TxOut(..), TxOutRef(..))
import Plutus.V1.Ledger.TxId (TxId(..))
import Plutus.V1.Ledger.Value (
  AssetClass(..),
  CurrencySymbol(..),
  TokenName(..),
  Value(..),
 )

import Test.QuickCheck.Plutus.Instances ()
import Test.QuickCheck (Arbitrary(arbitrary),oneof)

instance Arbitrary Ada where
  arbitrary = Lovelace <$> arbitrary

instance Arbitrary ScriptPurpose where
  arbitrary = oneof
   [ Minting <$> arbitrary
   , Spending <$> arbitrary
   , Rewarding <$> arbitrary
   , Certifying <$> arbitrary
   ]

instance Arbitrary ScriptContext where
  arbitrary = ScriptContext <$> arbitrary <*> arbitrary

instance Arbitrary TxInfo where
  arbitrary = TxInfo
          <$> arbitrary
          <*> arbitrary
          <*> arbitrary
          <*> arbitrary
          <*> arbitrary
          <*> arbitrary
          <*> arbitrary
          <*> arbitrary
          <*> arbitrary
          <*> arbitrary

instance Arbitrary TxInInfo where
  arbitrary = TxInInfo <$> arbitrary <*> arbitrary

instance Arbitrary PrivateKey where
  arbitrary = PrivateKey <$> arbitrary

instance Arbitrary DCert where
  arbitrary = oneof
    [ DCertDelegRegKey <$> arbitrary
    , DCertDelegDeRegKey <$> arbitrary
    , DCertDelegDelegate <$> arbitrary <*> arbitrary
    , DCertPoolRegister <$> arbitrary <*> arbitrary
    , DCertPoolRetire <$> arbitrary <*> arbitrary
    , pure DCertGenesis
    , pure DCertMir
    ]

instance Arbitrary a => Arbitrary (Extended a) where
  arbitrary = oneof
    [ pure NegInf
    , Finite <$> arbitrary
    , pure PosInf
    ]

instance Arbitrary a => Arbitrary (LowerBound a) where
  arbitrary = LowerBound <$> arbitrary <*> arbitrary

instance Arbitrary a => Arbitrary (UpperBound a) where
  arbitrary = UpperBound <$> arbitrary <*> arbitrary

instance Arbitrary a => Arbitrary (Interval a) where
  arbitrary = Interval <$> arbitrary <*> arbitrary

instance Arbitrary Redeemer where
  arbitrary = Redeemer <$> arbitrary

instance Arbitrary MintingPolicyHash where
  arbitrary = MintingPolicyHash <$> arbitrary

instance Arbitrary ScriptHash where
  arbitrary = ScriptHash <$> arbitrary

instance Arbitrary StakeValidatorHash where
  arbitrary = StakeValidatorHash <$> arbitrary

instance Arbitrary Slot where
  arbitrary = Slot <$> arbitrary

data ALedgerType
 = AnAda Ada
 | AnAddress Address
 | SomeLedgerBytes LedgerBytes
 | AScriptContext ScriptContext
 | AScriptPurpose ScriptPurpose
 | ATxInInfo TxInInfo
 | ATxInfo TxInfo
 | ACredential Credential
 | AStakingCredential StakingCredential
 | APrivateKey PrivateKey
 | APubKey PubKey
 | APubKeyHash PubKeyHash
 | ASignature Signature
 | ADCert DCert
 | AnInterval (Interval POSIXTime)
 | ALowerBound (LowerBound POSIXTime)
 | AnUpperBound (UpperBound POSIXTime)
 | ADatum Datum
 | ADatumHash DatumHash
 -- | AMintingPolicy MintingPolicy
 | AMintingPolicyHash MintingPolicyHash
 | ARedeemer Redeemer
 | AScriptHash ScriptHash
 | AStaKeValidatorHash StakeValidatorHash
 -- | AValidator Validator
 | AValidatorHash ValidatorHash
 | ASlot Slot
 | ADiffMilliSeconds DiffMilliSeconds
 | APOSIXTime POSIXTime
 | ATxOut TxOut
 | ATxOutRef TxOutRef
 | ATxId TxId
 | AnAssetClass AssetClass
 | ACurrencySymbol CurrencySymbol
 | ATokenName TokenName
 | AValue Value

instance Arbitrary ALedgerType where
  arbitrary = oneof
    [ AnAda <$> arbitrary
    , AnAddress <$> arbitrary
    , SomeLedgerBytes <$> arbitrary
    , AScriptContext <$> arbitrary
    , AScriptPurpose <$> arbitrary
    , ATxInInfo <$> arbitrary
    , ATxInfo <$> arbitrary
    , ACredential <$> arbitrary
    , AStakingCredential <$> arbitrary
    , APrivateKey <$> arbitrary
    , APubKey <$> arbitrary
    , APubKeyHash <$> arbitrary
    , ASignature <$> arbitrary
    , ADCert <$> arbitrary
    , AnInterval <$> arbitrary
    , ALowerBound <$> arbitrary
    , AnUpperBound <$> arbitrary
    , ADatum <$> arbitrary
    , ADatumHash <$> arbitrary
    --, AMintingPolicy <$> arbitrary
    , AMintingPolicyHash <$> arbitrary
    , ARedeemer <$> arbitrary
    , AScriptHash <$> arbitrary
    , AStaKeValidatorHash <$> arbitrary
    --, AValidator <$> arbitrary
    , AValidatorHash <$> arbitrary
    , ASlot <$> arbitrary
    , ADiffMilliSeconds <$> arbitrary
    , APOSIXTime <$> arbitrary
    , ATxOut <$> arbitrary
    , ATxOutRef <$> arbitrary
    , ATxId <$> arbitrary
    , AnAssetClass <$> arbitrary
    , ACurrencySymbol <$> arbitrary
    , ATokenName <$> arbitrary
    , AValue <$> arbitrary
    ]
