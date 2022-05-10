module Test.QuickCheck.Combinators where

import Test.QuickCheck (class Arbitrary, arbitrary, Result (..))
import Test.QuickCheck.Gen (chooseInt, sized)

import Prelude hiding (conj, disj)
import Data.Tuple (Tuple (..))
import Data.Generic.Rep (class Generic)
import Data.Typelevel.Num (class Nat, toInt')
import Data.Unfoldable (class Unfoldable, replicateA)
import Data.Traversable (class Traversable)
import Type.Proxy (Proxy (..))


-- | Combine two results with "And" logic, and with `", and "` as the failure message separator
conj :: Result -> Result -> Result
conj = conj' ", and "

-- | Combine two results with "And" logic, and with a failure message separator
conj' :: String -> Result -> Result -> Result
conj' m x y = case Tuple x y of
  Tuple Success Success -> Success
  Tuple (Failed x') (Failed y') -> Failed (x' <> m <> y')
  Tuple Success y' -> y'
  Tuple x' Success -> x'

infixr 3 conj as &=&


-- | Combine two results with "Or" logic, and with `", or "` as the failure message separator
disj :: Result -> Result -> Result
disj = disj' ", or "

-- | Combine two results with "Or" logic, and with a failure message separator
disj' :: String -> Result -> Result -> Result
disj' m x y = case Tuple x y of
  Tuple (Failed x') (Failed y') -> Failed (x' <> m <> y')
  _ -> Success

infixr 3 disj as |=|


-- | Combine two results with "Exclusive Or" logic, and with `", xor "` as the failure message separator, and "XOR" as the failure message if they are both `Success`
xor :: Result -> Result -> Result
xor = xor' ", xor " "XOR"


-- | Combine two results with "Exclusive Or" logic, and with a failure message separator and failure message if they are both `Success`
xor' :: String -- ^ Separator
     -> String -- ^ Success failure message
     -> Result -> Result -> Result
xor' m s x y = case Tuple x y of
  Tuple (Failed x') (Failed y') -> Failed (x' <> m <> y')
  Tuple Success Success -> Failed s
  Tuple Success y' -> y'
  Tuple x' Success -> x'


infixr 3 xor as |-|


-- | Uses the second failure message as the result failure message
implies :: Result -> Result -> Result
implies x y = case y of
  Failed y' -> case x of
    Success -> Failed ("Implied failure: " <> y')
    _ -> Success
  _ -> Success


infixr 3 implies as ==>


-- | Supply a failure message if successful
not' :: String -> Result -> Result
not' m x = case x of
  Success -> Failed m
  _ -> Success





newtype AtLeast n t a = AtLeast (t a)
getAtLeast :: forall n t a. AtLeast n t a -> t a
getAtLeast (AtLeast xs) = xs

derive instance genericAtLeast :: (Generic (t a) ta) => Generic (AtLeast n t a) _

instance arbitraryAtLeast :: (Arbitrary a, Nat n, Unfoldable t, Traversable t) => Arbitrary (AtLeast n t a) where
  arbitrary = sized \s -> do
    let n = toInt' (Proxy :: Proxy n)
    l <- chooseInt n (if s < n then n else s)
    AtLeast <$> replicateA l arbitrary


newtype AtMost n t a = AtMost (t a)
getAtMost :: forall n t a. AtMost n t a -> t a
getAtMost (AtMost xs) = xs

derive instance genericAtMost :: (Generic (t a) ta) => Generic (AtMost n t a) _

instance arbitraryAtMost :: (Arbitrary a, Nat n, Unfoldable t, Traversable t) => Arbitrary (AtMost n t a) where
  arbitrary = do
    let n = toInt' (Proxy :: Proxy n)
    l <- chooseInt 0 n
    AtMost <$> replicateA l arbitrary


newtype Between n m t a = Between (t a)
getBetween :: forall n m t a. Between n m t a -> t a
getBetween (Between xs) = xs

derive instance genericBetween :: (Generic (t a) ta) => Generic (Between n m t a) _

instance arbitraryBetween :: (Arbitrary a, Nat n, Nat m, Unfoldable t, Traversable t) => Arbitrary (Between n m t a) where
  arbitrary = do
    let n = toInt' (Proxy :: Proxy n)
        m = toInt' (Proxy :: Proxy m)
    l <- chooseInt n m
    Between <$> replicateA l arbitrary
