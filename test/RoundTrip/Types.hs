{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-missing-import-lists #-}

module RoundTrip.Types where

import Data.Aeson (FromJSON, ToJSON)
import Data.Map (Map)
import Data.Set (Set)
import GHC.Generics (Generic)
import Test.QuickCheck (Arbitrary (..), chooseEnum, oneof, resize, sized)
import qualified PlutusTx as P
import PlutusTx.Aux
import PlutusTx.ConstrIndices

data TestData
  = Maybe (Maybe TestSum)
  | Either (Either (Maybe Int) (Maybe Bool))
  deriving stock (Show, Eq, Ord, Generic)

instance FromJSON TestData

instance ToJSON TestData

instance Arbitrary TestData where
  arbitrary =
    oneof
      [ Maybe <$> arbitrary
      , Either <$> arbitrary
      ]

data TestSum
  = Nullary
  | Bool Bool
  | Int Int
  | Number Double
  | String String
  | Array [Int]
  | InlineRecord {why :: String, wouldYouDoThis :: Int}
  | MultiInlineRecords TestMultiInlineRecords
  | Record (TestRecord Int)
  | NestedRecord (TestRecord (TestRecord Int))
  | NT TestNewtype
  | NTRecord TestNewtypeRecord
  | TwoFields TestTwoFields
  | Set (Set Int)
  | Map (Map String Int)
  | Unit ()
  | MyUnit MyUnit
  | Pair (Int, Double)
  | Triple (Int, (), Bool)
  | Quad (Int, Double, Bool, Double)
  | QuadSimple Int Double Bool Double
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
      , Int <$> arbitrary
      , Number <$> arbitrary
      , String <$> arbitrary
      , Array <$> arbitrary
      , InlineRecord <$> arbitrary <*> arbitrary
      , MultiInlineRecords <$> arbitrary
      , Record <$> arbitrary
      , NestedRecord <$> arbitrary
      , NT <$> arbitrary
      , NTRecord <$> arbitrary
      , Map <$> arbitrary
      , Set <$> arbitrary
      , TwoFields <$> arbitrary
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
      { _foo1 :: Maybe Int
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
  { _field1 :: Maybe Int
  , _field2 :: a
  }
  deriving stock (Show, Eq, Ord, Generic)

instance (FromJSON a) => FromJSON (TestRecord a)

instance (ToJSON a) => ToJSON (TestRecord a)

instance (Arbitrary a) => Arbitrary (TestRecord a) where
  arbitrary = TestRecord <$> arbitrary <*> arbitrary

data TestTwoFields = TestTwoFields Bool Int
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
  deriving newtype (P.ToData,P.FromData,P.UnsafeFromData)

instance FromJSON ANewtype

instance ToJSON ANewtype

instance Arbitrary ANewtype where
  arbitrary = ANewtype <$> arbitrary

newtype ANewtypeRec = ANewtypeRec {ntrec :: ANewtype}
  deriving stock (Show, Eq, Generic)
  deriving newtype (P.ToData, P.FromData,P.UnsafeFromData)

instance Arbitrary ANewtypeRec where
  arbitrary = ANewtypeRec <$> arbitrary

instance FromJSON ANewtypeRec

instance ToJSON ANewtypeRec

data ARecord = ARecord {
    field1 :: Bool,
    field2 :: Either Bool [Bool],
    field3 :: Maybe Bool
  } deriving stock (Show, Eq, Generic)

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
  arbitrary
    = oneof
      [ ASumNT <$> arbitrary
      , ASumNTRec <$> arbitrary
      , ASumRec <$> arbitrary]

unstableMakeIsData ''ARecord
unstableMakeIsData ''ASum
