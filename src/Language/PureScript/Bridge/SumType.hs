{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module Language.PureScript.Bridge.SumType where

import Control.Lens (makeLenses, over)
import Data.Kind (Constraint, Type)
import Data.List (nub)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Map as Map
import Data.Maybe (maybeToList)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Typeable (Typeable)
import Generics.Deriving (
    C1,
    Constructor (conName),
    D1,
    Datatype,
    Generic (Rep, from),
    K1,
    M1 (M1),
    R,
    S1,
    Selector (selName),
    U1,
    type (:*:),
    type (:+:),
 )
import Language.PureScript.Bridge.TypeInfo (
    Language (..),
    TypeInfo (TypeInfo),
    flattenTypeInfo,
    mkTypeInfo,
    typeName,
 )

data ImportLine = ImportLine
    { importModule :: !Text
    , importTypes :: !(Set Text)
    }
    deriving (Eq, Ord, Show)

type ImportLines = Map Text ImportLine

-- | Generic representation of your Haskell types.
data SumType (lang :: Language)
    = SumType (TypeInfo lang) [(Int, DataConstructor lang)] [Instance lang]
    deriving (Show, Eq)

-- | TypeInfo lens for 'SumType'.
sumTypeInfo ::
    Functor f =>
    (TypeInfo lang -> f (TypeInfo lang)) ->
    SumType lang ->
    f (SumType lang)
sumTypeInfo inj (SumType info constrs is) =
    (\ti -> SumType ti constrs is) <$> inj info

-- | DataConstructor lens for 'SumType'.
sumTypeConstructors :: Applicative f => ([DataConstructor lang] -> f [DataConstructor lang]) -> SumType lang -> f (SumType lang)
sumTypeConstructors inj (SumType info constrs is) = (\cs -> SumType info cs is) <$> inj' constrs
  where
    inj' = fmap (uncurry zip) . traverse inj . unzip

{- | Create a representation of your sum (and product) types,
   for doing type translations and writing it out to your PureScript modules.
-}
mkSumType ::
    forall t.
    (Generic t, Typeable t, GDataConstructor (Rep t)) =>
    SumType 'Haskell
mkSumType = SumType (mkTypeInfo @t) constructors (Generic : maybeToList (nootype . map snd $ constructors))
  where
    constructors = zip [0 ..] $ gToConstructors (from (undefined :: t))

{- | Variant of @mkSumType@ which generates a purescript HasConstrIndex instance using the default ordering of
   constructors *without* a corresponding Haskell HasConstrIndices class. This should only be used if:

   1) You are certain the constructor order matches the "default" order

   2) You are certain the order of constructors will not change

   3) You are not able to generate a HasConstrIndices (or equivalent) instance, probably because the type you are
   trying to translate to purescript is defined in a plutus-tx library.
-}
extremelyUnsafeMkSumType ::
    forall t.
    (Generic t, Typeable t, GDataConstructor (Rep t)) =>
    SumType 'Haskell
extremelyUnsafeMkSumType = case mkSumType @t of
    SumType tInfo constructors instances -> SumType tInfo constructors (instances <> [HasConstrIndex])

{- | Variant of @mkSumType@ which constructs a SumType using a Haskell type class that can provide constructor
   index information.
-}
mkSumTypeIndexed ::
    forall (c :: Type -> Constraint) t.
    (Generic t, Typeable t, c t, GDataConstructor (Rep t)) =>
    (forall x. c x => [(Int, String)]) ->
    SumType 'Haskell
mkSumTypeIndexed f = SumType (mkTypeInfo @t) constructors (Generic : HasConstrIndex : maybeToList (nootype . map snd $ constructors))
  where
    ixs = M.fromList . map (\(i, t) -> (T.pack t, i)) $ f @t
    constructors =
        foldr
            ( \dcon@(DataConstructor name _) acc -> case M.lookup name ixs of
                -- we want to error here
                Nothing -> error . T.unpack $ "Constructor \"" <> name <> "\" does not have a specified index!"
                Just i -> (i, dcon) : acc
            )
            []
            $ gToConstructors (from (undefined :: t))

-- | Purescript typeclass instances that can be generated for your Haskell types.
data Instance (lang :: Language)
    = Generic
    | GenericShow
    | Json
    | Newtype
    | Functor
    | Eq
    | Eq1
    | Ord
    | Enum
    | Bounded
    | HasConstrIndex
    | ToData
    | FromData
    | Custom (CustomInstance lang)
    deriving (Eq, Show)

type PSInstance = Instance 'PureScript

data InstanceMember (lang :: Language) = InstanceMember
    { _memberName :: Text
    , _memberBindings :: [Text]
    , _memberBody :: Text
    , _memberDependencies :: [TypeInfo lang]
    , _memberImportLines :: ImportLines
    }
    deriving (Eq, Ord, Show)

data InstanceImplementation (lang :: Language)
    = Derive
    | DeriveNewtype
    | Explicit [InstanceMember lang]
    deriving (Eq, Ord, Show)

data CustomInstance (lang :: Language) = CustomInstance
    { _customConstraints :: [TypeInfo lang]
    , _customHead :: TypeInfo lang
    , _customImplementation :: InstanceImplementation lang
    }
    deriving (Eq, Ord, Show)

{- | The Purescript typeclass `Newtype` might be derivable if the original
 Haskell type was a simple type wrapper.
-}
nootype :: [DataConstructor lang] -> Maybe (Instance lang)
nootype [DataConstructor _ (Record _)] = Just Newtype
nootype [DataConstructor _ (Normal [_])] = Just Newtype
nootype _ = Nothing

-- | Ensure that aeson-compatible `EncodeJson` and `DecodeJson` instances are generated for your type.
argonaut :: SumType t -> SumType t
argonaut (SumType ti dc is) = SumType ti dc . nub $ Json : is

-- | Ensure that a generic `Show` instance is generated for your type.
genericShow :: SumType t -> SumType t
genericShow (SumType ti dc is) = SumType ti dc . nub $ GenericShow : is

{- | Ensure that a functor instance is generated for your type. It it
 your responsibility to ensure your type is a functor.
-}
functor :: SumType t -> SumType t
functor (SumType ti dc is) = SumType ti dc . nub $ Functor : is

-- | Ensure that an `Eq` instance is generated for your type.
equal :: SumType t -> SumType t
equal (SumType ti dc is) = SumType ti dc . nub $ Eq : is

-- | Ensure that an `Eq1` instance is generated for your type.
equal1 :: SumType t -> SumType t
equal1 (SumType ti dc is) = SumType ti dc . nub $ Eq1 : is

-- | Ensure that both `Eq` and `Ord` instances are generated for your type.
order :: SumType t -> SumType t
order (SumType ti dc is) = SumType ti dc . nub $ Eq : Ord : is

data DataConstructor (lang :: Language) = DataConstructor
    { -- | e.g. `Left`/`Right` for `Either`
      _sigConstructor :: !Text
    , _sigValues :: !(DataConstructorArgs lang)
    }
    deriving (Show, Eq)

data DataConstructorArgs (lang :: Language)
    = Nullary
    | Normal (NonEmpty (TypeInfo lang))
    | Record (NonEmpty (RecordEntry lang))
    deriving (Show, Eq)

instance Semigroup (DataConstructorArgs lang) where
    Nullary <> b = b
    a <> Nullary = a
    Normal as <> Normal bs = Normal $ as <> bs
    Record as <> Record bs = Record $ as <> bs
    Normal as <> Record bs = Normal as <> Normal (_recValue <$> bs)
    Record as <> Normal bs = Normal (_recValue <$> as) <> Normal bs

instance Monoid (DataConstructorArgs lang) where
    mempty = Nullary

data RecordEntry (lang :: Language) = RecordEntry
    { -- | e.g. `runState` for `State`
      _recLabel :: !Text
    , _recValue :: !(TypeInfo lang)
    }
    deriving (Show, Eq)

class GDataConstructor f where
    gToConstructors :: f a -> [DataConstructor 'Haskell]

class GDataConstructorArgs f where
    gToDataConstructorArgs :: f a -> DataConstructorArgs 'Haskell

instance (Datatype a, GDataConstructor c) => GDataConstructor (D1 a c) where
    gToConstructors (M1 c) = gToConstructors c

instance (GDataConstructor a, GDataConstructor b) => GDataConstructor (a :+: b) where
    gToConstructors _ =
        gToConstructors (undefined :: a f) ++ gToConstructors (undefined :: b f)

instance (Constructor a, GDataConstructorArgs b) => GDataConstructor (C1 a b) where
    gToConstructors c@(M1 r) =
        [DataConstructor{_sigConstructor = constructor, _sigValues = values}]
      where
        constructor = T.pack $ conName c
        values = gToDataConstructorArgs r

instance (GDataConstructorArgs a, GDataConstructorArgs b) => GDataConstructorArgs (a :*: b) where
    gToDataConstructorArgs _ =
        gToDataConstructorArgs (undefined :: a f) <> gToDataConstructorArgs (undefined :: b f)

instance GDataConstructorArgs U1 where
    gToDataConstructorArgs _ = mempty

instance (Selector a, Typeable t) => GDataConstructorArgs (S1 a (K1 R t)) where
    gToDataConstructorArgs e = case selName e of
        "" -> Normal [mkTypeInfo @t]
        name -> Record [RecordEntry (T.pack name) (mkTypeInfo @t)]

{- | Get all used types in a sum type.

   This includes all types found at the right hand side of a sum type
   definition, not the type parameters of the sum type itself
-}
getUsedTypes :: SumType lang -> Set (TypeInfo lang)
getUsedTypes (SumType _ cs is) =
    Set.fromList . concatMap flattenTypeInfo $
        concatMap (constructorToTypes . snd) cs <> concatMap instanceToTypes is

constructorToTypes :: DataConstructor lang -> [TypeInfo lang]
constructorToTypes (DataConstructor _ Nullary) = []
constructorToTypes (DataConstructor _ (Normal ts)) = NE.toList ts
constructorToTypes (DataConstructor _ (Record rs)) = _recValue <$> NE.toList rs

instanceToTypes :: Instance lang -> [TypeInfo lang]
instanceToTypes Generic = pure $ constraintToType $ TypeInfo "purescript-prelude" "Data.Generic.Rep" "Generic" []
instanceToTypes GenericShow = pure $ constraintToType $ TypeInfo "purescript-prelude" "Prelude" "Show" []
instanceToTypes Json =
    constraintToType
        <$> [ TypeInfo "purescript-argonaut-codecs" "Data.Argonaut.Decode" "DecodeJson" []
            , TypeInfo "purescript-argonaut-codecs" "Data.Argonaut.Encode" "EncodeJson" []
            ]
instanceToTypes Newtype =
    pure $ constraintToType $ TypeInfo "purescript-newtype" "Data.Newtype" "Newtype" []
instanceToTypes Functor =
    pure $ constraintToType $ TypeInfo "purescript-prelude" "Prelude" "Functor" []
instanceToTypes Eq =
    pure $ constraintToType $ TypeInfo "purescript-prelude" "Prelude" "Eq" []
instanceToTypes Eq1 =
    pure $ constraintToType $ TypeInfo "purescript-prelude" "Data.Eq" "Eq1" []
instanceToTypes Ord =
    pure $ constraintToType $ TypeInfo "purescript-prelude" "Prelude" "Ord" []
instanceToTypes Enum =
    pure $ constraintToType $ TypeInfo "purescript-enums" "Data.Enum" "Enum" []
instanceToTypes Bounded =
    pure $ constraintToType $ TypeInfo "purescript-prelude" "Prelude" "Bounded" []
-- fix this later (i don't think it matters now)
instanceToTypes HasConstrIndex =
    pure $ constraintToType $ TypeInfo "plutonomicon-cardano-transaction-lib" "ConstrIndices" "HasConstrIndices" []
instanceToTypes ToData =
    pure $ constraintToType $ TypeInfo "plutonomicon-cardano-transaction-lib" "ToData" "ToData" []
instanceToTypes FromData =
    pure $ constraintToType $ TypeInfo "plutonomicon-cardano-transaction-lib" "FromData" "FromData" []
instanceToTypes (Custom CustomInstance{..}) =
    constraintToType _customHead : (fmap constraintToType _customConstraints <> implementationToTypes _customImplementation)

constraintToType :: TypeInfo lang -> TypeInfo lang
constraintToType = over typeName ("class " <>)

implementationToTypes :: InstanceImplementation lang -> [TypeInfo lang]
implementationToTypes (Explicit members) = concatMap _memberDependencies members
implementationToTypes _ = []

instanceToImportLines :: PSInstance -> ImportLines
instanceToImportLines GenericShow =
    importsFromList [ImportLine "Data.Show.Generic" $ Set.singleton "genericShow"]
instanceToImportLines Json =
    importsFromList
        [ ImportLine "Control.Lazy" $ Set.singleton "defer"
        , ImportLine "Data.Argonaut.Core" $ Set.fromList ["jsonNull"]
        , ImportLine "Data.Argonaut.Encode" $ Set.fromList ["encodeJson"]
        , ImportLine "Data.Argonaut.Decode" $ Set.fromList ["decodeJson"]
        , ImportLine "Data.Argonaut.Decode.Aeson" $ Set.fromList ["null", "decode", "(</$\\>)", "(</*\\>)", "(</\\>)"]
        , ImportLine "Data.Argonaut.Encode.Aeson" $ Set.fromList ["null", "encode", "(>$<)", "(>/\\<)"]
        , ImportLine "Data.Newtype" $ Set.singleton "unwrap"
        , ImportLine "Data.Tuple.Nested" $ Set.singleton "(/\\)"
        ]
instanceToImportLines Enum =
    importsFromList
        [ ImportLine "Data.Enum.Generic" $ Set.fromList ["genericPred", "genericSucc"]
        ]
instanceToImportLines Bounded =
    importsFromList
        [ ImportLine "Data.Bounded.Generic" $ Set.fromList ["genericBottom", "genericTop"]
        ]
instanceToImportLines HasConstrIndex =
    importsFromList
        [ ImportLine "ConstrIndices" $ Set.fromList ["constrIndices", "fromConstr2Index"]
        , ImportLine "Data.Tuple" $ Set.fromList ["Tuple(Tuple)"]
        ]
instanceToImportLines ToData =
    importsFromList
        [ ImportLine "ToData" $ Set.fromList ["toData", "genericToData"]
        ]
instanceToImportLines FromData =
    importsFromList
        [ ImportLine "FromData" $ Set.fromList ["fromData", "genericFromData"]
        ]
instanceToImportLines (Custom CustomInstance{_customImplementation = Explicit members}) =
    importsFromList $ concatMap (Map.elems . _memberImportLines) members
instanceToImportLines _ = Map.empty

importsFromList :: [ImportLine] -> Map Text ImportLine
importsFromList ls =
    let pairs = zip (importModule <$> ls) ls
        merge a b =
            ImportLine (importModule a) (importTypes a `Set.union` importTypes b)
     in Map.fromListWith merge pairs

-- Lenses:
makeLenses ''DataConstructor

makeLenses ''RecordEntry

makeLenses ''CustomInstance

makeLenses ''InstanceImplementation

makeLenses ''InstanceMember
