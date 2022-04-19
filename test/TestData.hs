{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# HLINT ignore SingleValueConstr #-}
{-# OPTIONS_GHC -Wno-missing-import-lists #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module TestData where

import Data.Functor.Classes (Eq1 (liftEq))
import Data.Proxy ()
import Data.Text (Text)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Language.PureScript.Bridge (
  BridgePart,
  DataConstructor,
  FullBridge,
  HasHaskType (haskType),
  HaskellType,
  Language (Haskell, PureScript),
  PSType,
  SumType (..),
  TypeInfo,
  bridgeSumType,
  buildBridge,
  defaultBridge,
  mkSumType,
  mkTypeInfo,
  typeModule,
  typeName,
  (<|>),
  (^==),
 )
import Language.PureScript.Bridge.PSTypes (psString)
import PlutusTx.Aux (mkIndicesDefault)
import PlutusTx.ConstrIndices (HasConstrIndices (getConstrIndices))

-- Check that examples compile:
textBridge :: BridgePart
textBridge = do
  typeName ^== "Text"
  typeModule ^== "Data.Text.Internal" <|> typeModule ^== "Data.Text.Internal.Lazy"
  return psString

stringBridge :: BridgePart
stringBridge = do
  haskType ^== mkTypeInfo @String
  return psString

data Foo
  = Foo
  | Bar Int
  | FooBar Int Text
  deriving stock (Eq, Ord, Generic, Typeable, Show)

data Func a = Func Int a
  deriving stock (Eq, Ord, Functor, Generic, Typeable, Show)

instance Eq1 Func where
  liftEq eq (Func n x) (Func m y) = n == m && x `eq` y

data Test
  = TestIntInt Int Int
  | TestBool {bool :: Bool}
  | TestVoid
  deriving stock (Generic, Typeable, Show)

data Bar a b m c
  = Bar1 (Maybe a)
  | Bar2 (Either a b)
  | Bar3 a
  | Bar4 {myMonadicResult :: m b}
  deriving stock (Generic, Typeable, Show)

data SingleRecord a b = SingleRecord
  { _a :: a
  , _b :: b
  , c :: String
  }
  deriving stock (Generic, Eq, Ord, Typeable, Show)

data TwoRecords
  = FirstRecord
      { _fra :: String
      , _frb :: Int
      }
  | SecondRecord
      { _src :: Int
      , _srd :: [Int]
      }
  deriving stock (Generic, Typeable, Show)

mkIndicesDefault ''TwoRecords

newtype SomeNewtype = SomeNewtype Int
  deriving stock (Generic, Typeable, Show)

data SingleValueConstr = SingleValueConstr Int
  deriving stock (Generic, Typeable, Show)

data SingleProduct = SingleProduct Text Int
  deriving stock (Generic, Typeable, Show)

a :: HaskellType
a = mkTypeInfo @(Either String Int)

applyBridge :: FullBridge
applyBridge = buildBridge defaultBridge

psA :: PSType
psA = applyBridge a

b :: SumType 'Haskell
b = mkSumType @(Either String Int)

t :: TypeInfo 'PureScript
cs :: [(Int, DataConstructor 'PureScript)]
psB :: SumType 'PureScript
psB@(SumType t cs _) = bridgeSumType (buildBridge defaultBridge) b
