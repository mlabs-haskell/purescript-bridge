{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

-- | PureScript types to be used for bridges, e.g. in "Language.PureScript.Bridge.Primitives".
module Language.PureScript.Bridge.PSTypes (
  psArray,
  psBool,
  psEither,
  psInt,
  psNumber,
  psMaybe,
  psString,
  psTuple,
  psMap,
  psSet,
  psUnit,
) where

import Control.Lens (view)
import Control.Monad.Reader.Class (MonadReader)
import Language.PureScript.Bridge.Builder (
  BridgeData,
  fullBridge,
  psTypeParameters,
 )
import Language.PureScript.Bridge.TypeInfo (
  HasHaskType (haskType),
  PSType,
  TypeInfo (
    TypeInfo,
    _typeModule,
    _typeName,
    _typePackage,
    _typeParameters
  ),
  typeParameters,
 )

-- | Uses  type parameters from 'haskType' (bridged).
psArray :: MonadReader BridgeData m => m PSType
psArray = TypeInfo "" "Prim" "Array" <$> psTypeParameters

psBool :: PSType
psBool =
  TypeInfo
    { _typePackage = ""
    , _typeModule = "Prim"
    , _typeName = "Boolean"
    , _typeParameters = []
    }

-- | Uses  type parameters from 'haskType' (bridged).
psEither :: MonadReader BridgeData m => m PSType
psEither =
  TypeInfo "purescript-either" "Data.Either" "Either" <$> psTypeParameters

psInt :: PSType
psInt =
  TypeInfo
    { _typePackage = ""
    , _typeModule = "Prim"
    , _typeName = "Int"
    , _typeParameters = []
    }

psNumber :: PSType
psNumber =
  TypeInfo
    { _typePackage = ""
    , _typeModule = "Prim"
    , _typeName = "Number"
    , _typeParameters = []
    }

-- | Uses  type parameters from 'haskType' (bridged).
psMaybe :: MonadReader BridgeData m => m PSType
psMaybe = TypeInfo "purescript-maybe" "Data.Maybe" "Maybe" <$> psTypeParameters

psString :: PSType
psString =
  TypeInfo
    { _typePackage = ""
    , _typeModule = "Prim"
    , _typeName = "String"
    , _typeParameters = []
    }

-- | Uses  type parameters from 'haskType' (bridged).
psTuple :: MonadReader BridgeData m => m PSType
psTuple = do
  params <- view (haskType . typeParameters)
  bridge <- view fullBridge
  let computeTuple [] = psUnit
      computeTuple [a] = bridge a
      computeTuple [a, b] = TypeInfo "purescript-tuples" "Data.Tuple" "Tuple" [bridge a, bridge b]
      computeTuple (h : t) = TypeInfo "purescript-tuples" "Data.Tuple" "Tuple" [bridge h, computeTuple t]
  pure $ computeTuple params

psUnit :: PSType
psUnit =
  TypeInfo
    { _typePackage = "purescript-prelude"
    , _typeModule = "Prelude"
    , _typeName = "Unit"
    , _typeParameters = []
    }

psMap :: MonadReader BridgeData m => m PSType
psMap =
  TypeInfo "purescript-ordered-collections" "Data.Map" "Map" <$> psTypeParameters

psSet :: MonadReader BridgeData m => m PSType
psSet =
  TypeInfo "purescript-ordered-collections" "Data.Set" "Set" <$> psTypeParameters
