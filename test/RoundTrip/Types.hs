{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}

module RoundTrip.Types (
  TestData (..),
  RepType (..),
  Request (..),
  Response (..),
  response,
  ANewtype (..),
  ANewtypeRec (..),
  ARecord (..),
  ASum (..),
  MyUnit (..),
  TestEnum (..),
  TestMultiInlineRecords (..),
  TestNewtype (..),
  TestNewtypeRecord (..),
  TestRecord (..),
  TestRecursiveA (..),
  TestRecursiveB (..),
  TestSum (..),
  TestTwoFields (..),
  TestPlutusData (..),
  TypesWithMap (..),
) where

import ArbitraryLedger (FixMap (fixMap), reMap)
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Plutus.V1.Ledger.Api (Credential, Datum, Interval, TxInfo(..),POSIXTime, ScriptContext(..),TxInInfo(..),TxOutRef(..))
import Plutus.V1.Ledger.Value (Value)
import PlutusTx qualified as P
import PlutusTx.AssocMap qualified as AssocMap
import PlutusTx.Aux (unstableMakeIsData)
import PlutusTx.ConstrIndices (HasConstrIndices (getConstrIndices))
import PlutusTx.Ratio (Rational)
import Test.QuickCheck (Arbitrary (arbitrary), chooseEnum, oneof, resize, sized)
import Test.QuickCheck.Plutus.Modifiers (UniqueList (UniqueList), uniqueListOf)

data TestData
  = Maybe (Maybe TestSum)
  | Either (Either (Maybe Bool) (Maybe Bool))
  | PlutusData TestPlutusData
  deriving stock (Show, Eq, Generic)

instance FromJSON TestData

instance ToJSON TestData

instance Arbitrary TestData where
  arbitrary =
    oneof
      [ Maybe <$> arbitrary
      , Either <$> arbitrary
      , PlutusData <$> arbitrary
      ]

data TestSum
  = Nullary
  | Bool Bool
  | Int Integer
  | Number Double
  | String String
  | Array [Bool]
  | InlineRecord {why :: String, wouldYouDoThis :: Bool}
  | MultiInlineRecords TestMultiInlineRecords
  | Record (TestRecord Bool)
  | NestedRecord (TestRecord (TestRecord Bool))
  | NT TestNewtype
  | NTRecord TestNewtypeRecord
  | TwoFields TestTwoFields
  | --  | Set (Set Bool)
    --    Map (Map String Bool)
    Unit ()
  | MyUnit MyUnit
  | Pair (Bool, Double)
  | Triple (Bool, (), Integer)
  | Quad (Bool, Double, Integer, Double)
  | QuadSimple Bool Double Bool Double
  | Recursive TestRecursiveA
  | Enum TestEnum
  deriving stock (Show, Eq, Ord, Generic)

instance FromJSON TestSum

instance ToJSON TestSum

instance Arbitrary TestSum where
  arbitrary =
    oneof
      [ pure Nullary
      , Bool <$> arbitrary
      , Bool <$> arbitrary
      , Number <$> arbitrary
      , String <$> arbitrary
      , Array <$> arbitrary
      , InlineRecord <$> arbitrary <*> arbitrary
      , MultiInlineRecords <$> arbitrary
      , Record <$> arbitrary
      , NestedRecord <$> arbitrary
      , NT <$> arbitrary
      , NTRecord <$> arbitrary
      , --      , Map <$> arbitrary
        TwoFields <$> arbitrary
      , pure $ Unit ()
      , Pair <$> arbitrary
      , Triple <$> arbitrary
      , Quad <$> arbitrary
      , QuadSimple <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
      , Enum <$> arbitrary
      ]

data TestRecursiveA = Nil | Recurse TestRecursiveB
  deriving stock (Show, Eq, Ord, Generic)

instance FromJSON TestRecursiveA

instance ToJSON TestRecursiveA

instance Arbitrary TestRecursiveA where
  arbitrary = sized go
    where
      go size
        | size > 0 = oneof [pure Nil, resize (size - 1) $ Recurse <$> arbitrary]
        | otherwise = pure Nil

newtype TestRecursiveB = RecurseB TestRecursiveB
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (Arbitrary)

instance FromJSON TestRecursiveB

instance ToJSON TestRecursiveB

data TestMultiInlineRecords
  = Foo
      { _foo1 :: Maybe Bool
      , _foo2 :: ()
      }
  | Bar
      { _bar1 :: String
      , _bar2 :: Bool
      }
  deriving stock (Show, Eq, Ord, Generic)

instance FromJSON TestMultiInlineRecords

instance ToJSON TestMultiInlineRecords

instance Arbitrary TestMultiInlineRecords where
  arbitrary =
    oneof
      [ Foo <$> arbitrary <*> arbitrary
      , Bar <$> arbitrary <*> arbitrary
      ]

data TestRecord a = TestRecord
  { _field1 :: Maybe Bool
  , _field2 :: a
  }
  deriving stock (Show, Eq, Ord, Generic)

instance (FromJSON a) => FromJSON (TestRecord a)

instance (ToJSON a) => ToJSON (TestRecord a)

instance (Arbitrary a) => Arbitrary (TestRecord a) where
  arbitrary = TestRecord <$> arbitrary <*> arbitrary

data TestTwoFields = TestTwoFields Bool Bool
  deriving stock (Show, Eq, Ord, Generic)

instance FromJSON TestTwoFields

instance ToJSON TestTwoFields

instance Arbitrary TestTwoFields where
  arbitrary = TestTwoFields <$> arbitrary <*> arbitrary

newtype TestNewtype = TestNewtype (TestRecord Bool)
  deriving stock (Show, Eq, Ord, Generic)

instance FromJSON TestNewtype

instance ToJSON TestNewtype

instance Arbitrary TestNewtype where
  arbitrary = TestNewtype <$> arbitrary

newtype TestNewtypeRecord = TestNewtypeRecord {unTestNewtypeRecord :: TestNewtype}
  deriving stock (Show, Eq, Ord, Generic)

instance FromJSON TestNewtypeRecord

instance ToJSON TestNewtypeRecord

instance Arbitrary TestNewtypeRecord where
  arbitrary = TestNewtypeRecord <$> arbitrary

data TestEnum
  = Mon
  | Tue
  | Wed
  | Thu
  | Fri
  | Sat
  | Sun
  deriving stock (Show, Eq, Ord, Bounded, Enum, Generic)

instance FromJSON TestEnum

instance ToJSON TestEnum

instance Arbitrary TestEnum where
  arbitrary = chooseEnum (minBound, maxBound)

data MyUnit = U deriving stock (Show, Eq, Ord, Bounded, Enum, Generic)

instance FromJSON MyUnit

instance ToJSON MyUnit

instance Arbitrary MyUnit where
  arbitrary = pure U

newtype ANewtype = ANewtype Bool
  deriving stock (Show, Eq, Generic)
  deriving newtype (P.ToData, P.FromData, P.UnsafeFromData, FromJSON, ToJSON)

instance Arbitrary ANewtype where
  arbitrary = ANewtype <$> arbitrary

newtype ANewtypeRec = ANewtypeRec {ntrec :: ANewtype}
  deriving stock (Show, Eq, Generic)
  deriving newtype (P.ToData, P.FromData, P.UnsafeFromData, FromJSON, ToJSON)

instance Arbitrary ANewtypeRec where
  arbitrary = ANewtypeRec <$> arbitrary

data ARecord = ARecord
  { field1 :: Bool
  , field2 :: Either Bool [Bool]
  , field3 :: Maybe Bool
  }
  deriving stock (Show, Eq, Generic)

instance Arbitrary ARecord where
  arbitrary = ARecord <$> arbitrary <*> arbitrary <*> arbitrary

instance FromJSON ARecord

instance ToJSON ARecord

data ASum
  = ASumNT ANewtype
  | ASumNTRec ANewtypeRec
  | ASumRec ARecord
  deriving stock (Show, Eq, Generic)

instance FromJSON ASum

instance ToJSON ASum

instance Arbitrary ASum where
  arbitrary =
    oneof
      [ ASumNT <$> arbitrary
      , ASumNTRec <$> arbitrary
      , ASumRec <$> arbitrary
      ]

data TestPlutusData
  = PdTypesWithMap TypesWithMap
  | PdRational PlutusTx.Ratio.Rational
  deriving stock (Show, Eq, Generic)

instance SOP.Generic TestPlutusData

data TypesWithMap
  = PdValue Value
  | PdScriptContext ScriptContext
  | PdInterval (Interval POSIXTime)
  | PdDatum Datum
  | PdMap (AssocMap.Map Integer Integer) -- (AssocMap.Map Integer Value)
  | PdCredential Credential
  deriving stock (Show, Eq, Generic)

instance SOP.Generic TypesWithMap

aBigInteger :: Integer
aBigInteger = (2 :: Integer) ^ (126 :: Integer)

overflowTxOutRefIdx :: ScriptContext -> ScriptContext
overflowTxOutRefIdx (ScriptContext ctx ps)  = ScriptContext (ctx {txInfoInputs = map go $ txInfoInputs ctx}) ps
  where
    go :: TxInInfo -> TxInInfo
    go ininfo = ininfo {txInInfoOutRef = go' $ txInInfoOutRef ininfo }
     where
       go' :: TxOutRef -> TxOutRef
       go' outref = outref {txOutRefIdx = aBigInteger}

instance Arbitrary TypesWithMap where
  arbitrary =
    fixMap reMap
      =<< resize
        2
        ( oneof
            [ PdCredential <$> arbitrary
            , PdValue <$> arbitrary
            , (PdScriptContext . overflowTxOutRefIdx) <$> arbitrary
            , PdDatum <$> arbitrary
            , PdInterval <$> arbitrary
            , PdMap <$> do
                -- NOTE: Fails if not unique keys see https://github.com/ngua/cardano-serialization-lib/blob/8b7579084dd3eb401a14a3493aa2e91778d48b66/rust/src/plutus.rs#L901
                (UniqueList keys) <- uniqueListOf 5
                AssocMap.fromList . zip keys <$> arbitrary
            ]
        )

instance Arbitrary TestPlutusData where
  arbitrary =
    oneof
      [ PdTypesWithMap <$> arbitrary
      , PdRational <$> arbitrary
      ]

unstableMakeIsData ''TestPlutusData
unstableMakeIsData ''TypesWithMap

instance FromJSON TestPlutusData

instance ToJSON TestPlutusData

instance FromJSON TypesWithMap

instance ToJSON TypesWithMap

-- | IPC round trip protocol messages
data RepType = RTJson | RTPlutusData deriving stock (Show, Eq, Generic)

instance FromJSON RepType
instance ToJSON RepType

data Request = Req RepType String
  deriving stock (Show, Eq, Generic)

instance FromJSON Request
instance ToJSON Request

data Response = RespSuccess RepType String | RespError String
  deriving stock (Show, Eq, Generic)

instance FromJSON Response
instance ToJSON Response

response :: forall p. (String -> p) -> (String -> p) -> (String -> p) -> Response -> p
response e _ _ (RespError err) = e err
response _ js _ (RespSuccess RTJson payload) = js payload
response _ _ pd (RespSuccess RTPlutusData payload) = pd payload

-- NOTE: I have to put TH stuff here because otherwise Haskell can't find local
-- type definitions
unstableMakeIsData ''ARecord
unstableMakeIsData ''ASum
