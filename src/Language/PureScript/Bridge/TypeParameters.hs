{-# LANGUAGE EmptyDataDeriving #-}

{- | As we translate types and not type constructors, we have to pass dummy types
   to any type constructor.

   'buildBridge' will translate all parameter types which
   come from a module TypeParameters (e.g. this one) to lower case.

   For translating something like Maybe:

   @
     data Maybe' a = Nothing' | Just' a
   @

   you would use:

   @
     import "Language.PureScript.Bridge"
     import "Language.PureScript.Bridge.TypeParameters"

     st = mkSumType @(Maybe' A) -- Note that we use "Maybe' A" instead of just Maybe - which would not work.
   @
-}
module Language.PureScript.Bridge.TypeParameters (
  A,
  B,
  C,
  D,
  E,
  F,
  G,
  H,
  I,
  J,
  K,
  L,
  M,
  N,
  O,
  P,
  Q,
  R,
  S,
  T,
  U,
  V,
  W,
  X,
  Y,
  Z,
  A1,
  B1,
  C1,
  D1,
  E1,
  F1,
  G1,
  H1,
  I1,
  J1,
  K1,
  L1,
  M1,
  N1,
  O1,
  P1,
  Q1,
  R1,
  S1,
  T1,
  U1,
  V1,
  W1,
  X1,
  Y1,
  Z1,
) where

data A deriving stock (Eq, Ord)

data B deriving stock (Eq, Ord)

data C deriving stock (Eq, Ord)

data D deriving stock (Eq, Ord)

data E deriving stock (Eq, Ord)

data F deriving stock (Eq, Ord)

data G deriving stock (Eq, Ord)

data H deriving stock (Eq, Ord)

data I deriving stock (Eq, Ord)

data J deriving stock (Eq, Ord)

data K deriving stock (Eq, Ord)

data L deriving stock (Eq, Ord)

data M deriving stock (Eq, Ord)

data N deriving stock (Eq, Ord)

data O deriving stock (Eq, Ord)

data P deriving stock (Eq, Ord)

data Q deriving stock (Eq, Ord)

data R deriving stock (Eq, Ord)

data S deriving stock (Eq, Ord)

data T deriving stock (Eq, Ord)

data U deriving stock (Eq, Ord)

data V deriving stock (Eq, Ord)

data W deriving stock (Eq, Ord)

data X deriving stock (Eq, Ord)

data Y deriving stock (Eq, Ord)

data Z deriving stock (Eq, Ord)

{- | You can use those if your type parameters are actually type constructors as well:
   @
   st = mkSumType @('ReaderT' R M1 A)
   @
-}
data A1 a

data B1 a

data C1 a

data D1 a

data E1 a

data F1 a

data G1 a

data H1 a

data I1 a

data J1 a

data K1 a

data L1 a

data M1 a

data N1 a

data O1 a

data P1 a

data Q1 a

data R1 a

data S1 a

data T1 a

data U1 a

data V1 a

data W1 a

data X1 a

data Y1 a

data Z1 a
