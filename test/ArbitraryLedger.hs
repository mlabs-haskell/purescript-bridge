{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module ArbitraryLedger (ALedgerType (..), WEq ((@==)), FixMap (fixMap), reMap) where

-- Ledger type imports

import Data.Bifunctor (bimap)
import Data.Function (on)
import Data.Kind (Type)
import Data.List (sort)
import Data.Map qualified as M
import GHC.Generics qualified as GHC
import Generics.SOP (All, All2, Code, Generic, I (I), K (K), NS (S, Z), Proxy (Proxy), SListI, SOP (SOP), from, hcliftA2, hcollapse, hctraverse', to)
import Plutus.V1.Ledger.Ada (Ada (Lovelace))
import Plutus.V1.Ledger.Address (Address)
import Plutus.V1.Ledger.Bytes (LedgerBytes)
import Plutus.V1.Ledger.Contexts (ScriptContext (ScriptContext), ScriptPurpose (Certifying, Minting, Rewarding, Spending), TxInInfo (TxInInfo), TxInfo (TxInfo))
import Plutus.V1.Ledger.Credential (Credential, StakingCredential)
import Plutus.V1.Ledger.Crypto (PrivateKey (PrivateKey), PubKey, PubKeyHash, Signature)
import Plutus.V1.Ledger.DCert (DCert (DCertDelegDeRegKey, DCertDelegDelegate, DCertDelegRegKey, DCertGenesis, DCertMir, DCertPoolRegister, DCertPoolRetire))
import Plutus.V1.Ledger.Interval (Extended (Finite, NegInf, PosInf), Interval (Interval), LowerBound (LowerBound), UpperBound (UpperBound))
import Plutus.V1.Ledger.Scripts (
  Datum (Datum),
  DatumHash,
  MintingPolicyHash (MintingPolicyHash),
  Redeemer (Redeemer),
  ScriptHash (ScriptHash),
  StakeValidatorHash (StakeValidatorHash),
  ValidatorHash,
 )
import Plutus.V1.Ledger.Slot (Slot (Slot))
import Plutus.V1.Ledger.Time (DiffMilliSeconds, POSIXTime)
import Plutus.V1.Ledger.Tx (TxOut, TxOutRef)
import Plutus.V1.Ledger.TxId (TxId)
import Plutus.V1.Ledger.Value (
  AssetClass,
  CurrencySymbol,
  TokenName,
  Value,
 )
import PlutusTx (unstableMakeIsData)
import PlutusTx qualified as P
import PlutusTx.AssocMap qualified as PMap
import PlutusTx.Builtins.Internal qualified as PI
import Test.QuickCheck (Arbitrary (arbitrary), Gen, oneof)
import Test.QuickCheck.Plutus.Instances ()
import Test.QuickCheck.Plutus.Modifiers (UniqueList (UniqueList), uniqueListOf)

instance Arbitrary Ada where
  arbitrary = Lovelace <$> arbitrary

instance Arbitrary ScriptPurpose where
  arbitrary =
    oneof
      [ Minting <$> arbitrary
      , Spending <$> arbitrary
      , Rewarding <$> arbitrary
      , Certifying <$> arbitrary
      ]

instance Arbitrary ScriptContext where
  arbitrary = ScriptContext <$> arbitrary <*> arbitrary

instance Arbitrary TxInfo where
  arbitrary =
    TxInfo
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
  arbitrary =
    oneof
      [ DCertDelegRegKey <$> arbitrary
      , DCertDelegDeRegKey <$> arbitrary
      , DCertDelegDelegate <$> arbitrary <*> arbitrary
      , DCertPoolRegister <$> arbitrary <*> arbitrary
      , DCertPoolRetire <$> arbitrary <*> arbitrary
      , pure DCertGenesis
      , pure DCertMir
      ]

instance Arbitrary a => Arbitrary (Extended a) where
  arbitrary =
    oneof
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
  | -- | AMintingPolicy MintingPolicy
    AMintingPolicyHash MintingPolicyHash
  | ARedeemer Redeemer
  | AScriptHash ScriptHash
  | AStaKeValidatorHash StakeValidatorHash
  | -- | AValidator Validator
    AValidatorHash ValidatorHash
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
  deriving stock (Show, Eq, GHC.Generic)

instance Arbitrary ALedgerType where
  arbitrary =
    oneof
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
      , --, AMintingPolicy <$> arbitrary
        AMintingPolicyHash <$> arbitrary
      , ARedeemer <$> arbitrary
      , AScriptHash <$> arbitrary
      , AStaKeValidatorHash <$> arbitrary
      , --, AValidator <$> arbitrary
        AValidatorHash <$> arbitrary
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

-- NOTE: seed = 1322691774
class WEq a where
  (@==) :: a -> a -> Bool

instance WEq Integer where
  (@==) = (==)

instance WEq PI.BuiltinByteString where
  (@==) = (==)

--AssocMap
instance (Eq v, Ord k) => WEq (PMap.Map k v) where
  m1 @== m2 = m1' == m2'
    where
      m1' = M.fromList $ PMap.toList m1
      m2' = M.fromList $ PMap.toList m2

normalize :: P.Data -> P.Data
normalize d = case d of
  P.Map m1 -> P.Map (sort . map (bimap normalize normalize) $ m1)
  P.Constr i1 ds1 -> P.Constr i1 (map normalize ds1)
  P.List ds1 -> P.List (map normalize ds1)
  other -> other

instance WEq P.Data where
  pa @== pb = normalize pa == normalize pb

instance (WEq a, WEq b) => WEq (a, b) where
  (a, b) @== (c, d) = a @== c && b @== d

instance WEq Datum where
  (Datum d1) @== (Datum d2) = d1 @== d2

instance WEq PI.BuiltinData where
  PI.BuiltinData d1 @== PI.BuiltinData d2 = d1 @== d2

instance (WEq a) => WEq [a] where
  [] @== [] = True
  (x : xs) @== (y : ys) = (x @== y) && xs @== ys
  _ @== _ = False

-- NOTE: If you derive SOP.Generic for a type (derive GHC.Generic then 'instance Generic MyType') which a SOP representation
-- such that all sums + products contain types with a WEq instance, that type will automatically be an instance of WEq as well
instance {-# INCOHERENT #-} (Generic a, All2 WEq (Code a)) => WEq a where
  a @== b = weq a b

-- adapted from basic-sop
weq :: (Generic a, All2 WEq (Code a)) => a -> a -> Bool
weq = go `on` from
  where
    go :: forall xss. (All2 WEq xss, All SListI xss) => SOP I xss -> SOP I xss -> Bool
    go (SOP (Z xs)) (SOP (Z ys)) = and . hcollapse $ hcliftA2 p eq xs ys
    go (SOP (S xss)) (SOP (S yss)) = go (SOP xss) (SOP yss)
    go _ _ = False

    p :: Proxy WEq
    p = Proxy

    eq :: forall (a :: Type). WEq a => I a -> I a -> K Bool a
    eq (I a) (I b) = K (a @== b)

-- this is basically a  Traversal over the list of tuples wrapped by a PlutusData Map constructor.
-- the idea is that, since I couldn't figure out how to write a non-broken arbitrary instance
-- (because we already have/need that broken instance in scope everywhere), we're going to use this class
-- + SOP generics to to rip apart generated instances and fix the maps using fixMap
class FixMap a where
  fixMap :: forall f. Monad f => ([(P.Data, P.Data)] -> f [(P.Data, P.Data)]) -> a -> f a

-- these are the "primitive" instances. some of these are functionally equivalent to 'pure'
-- (sorry kmett, no laws in this type class) but we need them as starter instances so we can
-- traverse types built up from them generically
instance FixMap P.Data where
  fixMap f = \case
    P.Constr i ds -> P.Constr i <$> traverse g ds
    P.Map ds -> P.Map <$> (traverse h ds >>= f)
    P.List ds -> P.List <$> traverse g ds
    other -> pure other
    where
      g = fixMap f
      h (a, b) = (,) <$> g a <*> g b

instance FixMap (PMap.Map k v) where
  fixMap _ = pure

instance FixMap PI.BuiltinByteString where
  fixMap _ = pure

instance (FixMap a, FixMap b) => FixMap (a, b) where
  fixMap f (a, b) = (,) <$> fixMap f a <*> fixMap f b

instance (FixMap a) => FixMap [a] where
  fixMap f = traverse (fixMap f)

instance FixMap PI.BuiltinData where
  fixMap f (PI.BuiltinData d) = PI.BuiltinData <$> fixMap f d

instance FixMap Datum where
  fixMap f (Datum d) = Datum <$> fixMap f d

instance FixMap Integer where
  fixMap _ i = pure i

-- this is incoherent because we want it to be the "last choice". morally abhorrent but practically necessary
-- i feel like this is actually a cool trick?
instance {-# INCOHERENT #-} (Generic a, All2 FixMap (Code a)) => FixMap a where
  fixMap = gfixMap

-- if every element in the SOP representation of a type has a FixMap instance, the whole type does
-- (every ledger type we bridge with a generic instance below should satisfy that, as should every type built from
-- those types provided any intermediary types also satisfy that constraint)
gfixMap :: forall a m. (Generic a, All2 FixMap (Code a), Monad m) => ([(P.Data, P.Data)] -> m [(P.Data, P.Data)]) -> a -> m a
gfixMap f = fmap to . go . from
  where
    go :: forall xss. (All2 FixMap xss, All SListI xss) => SOP I xss -> m (SOP I xss)
    go = hctraverse' (Proxy @FixMap) g
      where
        g :: forall (b :: Type). FixMap b => I b -> m (I b)
        g (I b) = I <$> fixMap f b

-- dumb newtype wrapper to generate orderly arbitrary AssocMap instances
newtype AssocMap = AssocMap {unMap :: PMap.Map Integer Value}

instance Arbitrary AssocMap where
  arbitrary =
    AssocMap <$> do
      (UniqueList keys) <- uniqueListOf 25
      (PMap.fromList . zip keys) <$> arbitrary

-- replace every argument to a Map constructor with the ToData'd contents of an arbitrarily generated AssocMap instance
-- if you want to generate maps some other way, you can write a function with this type signature and use it w/ fixmap to modify every
-- occurrence of a plutus data Map constructor in a given type (and all of the types it contains, recursively)
reMap :: [(P.Data, P.Data)] -> Gen [(P.Data, P.Data)]
reMap _ = do
  aMap <- unMap <$> arbitrary @AssocMap
  pure . map (bimap P.toData P.toData) . PMap.toList $ aMap

instance Generic (Interval a)
instance Generic POSIXTime
instance Generic ScriptContext
instance Generic (LowerBound a)
instance Generic (UpperBound a)
instance Generic (Extended a)
instance Generic Value
instance Generic Ada
instance Generic Address
instance Generic LedgerBytes
instance Generic ScriptPurpose
instance Generic TxInInfo
instance Generic TxInfo
instance Generic Credential
instance Generic StakingCredential
instance Generic PrivateKey
instance Generic PubKey
instance Generic PubKeyHash
instance Generic Signature
instance Generic DCert
instance Generic DatumHash
instance Generic MintingPolicyHash
instance Generic Redeemer
instance Generic ScriptHash
instance Generic StakeValidatorHash
instance Generic ValidatorHash
instance Generic Slot
instance Generic DiffMilliSeconds
instance Generic TxOut
instance Generic TxOutRef
instance Generic TxId
instance Generic AssetClass
instance Generic CurrencySymbol
instance Generic TokenName

unstableMakeIsData ''ALedgerType
