{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Language.PureScript.Bridge.Primitives (boolBridge, eitherBridge, setBridge, mapBridge, dummyBridge, intBridge, doubleBridge, maybeBridge, listBridge, stringBridge, textBridge, unitBridge, noContentBridge) where

import Control.Monad.Reader.Class (MonadReader)
import Language.PureScript.Bridge.Builder (
  BridgeData,
  BridgePart,
  clearPackageFixUp,
  (<|>),
  (^==),
 )
import Language.PureScript.Bridge.PSTypes (
  psArray,
  psBool,
  psEither,
  psInt,
  psMap,
  psMaybe,
  psNumber,
  psSet,
  psString,
  psUnit,
 )
import Language.PureScript.Bridge.TypeInfo (
  HasHaskType (haskType),
  PSType,
  mkTypeInfo,
  typeModule,
  typeName,
 )

boolBridge :: BridgePart
boolBridge = typeName ^== "Bool" >> return psBool

eitherBridge :: BridgePart
eitherBridge = typeName ^== "Either" >> psEither

setBridge :: BridgePart
setBridge = do
  typeName ^== "Set"
  typeModule ^== "Data.Set" <|> typeModule ^== "Data.Set.Internal"
  psSet

mapBridge :: BridgePart
mapBridge = do
  typeName ^== "Map"
  typeModule ^== "Data.Map" <|> typeModule ^== "Data.Map.Internal"
  psMap

-- | Dummy bridge, translates every type with 'clearPackageFixUp'
dummyBridge :: MonadReader BridgeData m => m PSType
dummyBridge = clearPackageFixUp

intBridge :: BridgePart
intBridge = typeName ^== "Int" >> return psInt

doubleBridge :: BridgePart
doubleBridge = typeName ^== "Double" >> return psNumber

listBridge :: BridgePart
listBridge = typeName ^== "[]" >> psArray

maybeBridge :: BridgePart
maybeBridge = typeName ^== "Maybe" >> psMaybe

stringBridge :: BridgePart
stringBridge =
  haskType ^== mkTypeInfo @String >> return psString

textBridge :: BridgePart
textBridge = do
  typeName ^== "Text"
  typeModule ^== "Data.Text.Internal"
    <|> typeModule ^== "Data.Text.Internal.Lazy"
  return psString

unitBridge :: BridgePart
unitBridge = typeName ^== "()" >> return psUnit

noContentBridge :: BridgePart
noContentBridge = typeName ^== "NoContent" >> return psUnit
