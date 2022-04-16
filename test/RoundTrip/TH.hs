{-# LANGUAGE LambdaCase  #-}

module RoundTrip.TH where

import Language.Haskell.TH.Syntax
import Language.Haskell.TH.DataType

import RoundTrip.Types
import Test.QuickCheck

import Data.IORef
import qualified Data.Map as M
import qualified Data.Set as S

-- someday I'm going to figure out when to use strict/lazy state monads
import Control.Monad.State.Strict
import Control.Monad.Trans.Class
import Control.Monad.IO.Class

import Control.Monad
import System.Random

newTyName :: Q Name
newTyName = newName "TYPE"

data TestState = TestState {
   -- a map that contains data declarations already generated.
   -- we need to keep track of them so we can generate a splice containing a Rec or HList
   -- of each data type we've generated
   testSumDecs        :: M.Map Name Dec,
   -- a random seed. we need this to generate constructor indices
   seed               :: StdGen,
   -- it's easier to make the HasConstrIndices instance at the same time as the data type, so we keep a list of them here
   hcInstances        :: [Dec]
   -- a set of already-used indices. we have to keep track of this so that
   -- we don't duplicate indices for "weird" stable index types
   staleConstrIndices :: S.Set Int,
 }

type DecState = StateT DataDecs Q

data IndexPlan = Normal | Weird deriving (Show, Eq)

-- we roll the dice to see if we're using a normal or weird constructor order
getIndexPlan :: DecState IndexPlan
getIndexPlan = do
  TestState ds g hc  is <- get
  let (isNormal,g') = random g
  modify $ const (TestState ds g' hc is)
  if isNormal
    then pure Normal
    else pure Weird

defaultBang :: Bang
defaultBang = Bang NoSourceUnpackedness NoSourceStrictness

mkBangType :: Type -> BangType
mkBangType t = (defaultBang,t)

defaultDerivClause :: DerivClause
defaultDerivClause
  = DerivClause Nothing $ map (ConT . mkName) ["Show","Eq","Generic","ToJSON","FromJSON"]

intT  = ConT . mkName $ "Int"
boolT = ConT . mkName $ "Bool"
doubleT = ConT . mkName $ "Double"
stringT = ConT . mkName $ "String"

listT :: Type -> Type
listT t = AppT ListT t

mapT :: Type -> Type -> Type
mapT k v =  AppT (AppT (ConT . mkName $ "Map") k) v

setT :: Type -> Type
setT t = AppT (ConT . mkName $ "Set") t

tupT :: Int -> Type
tupT n = TupleT n

unitT = tupT 0




-- Ignoring type var args, contexts, GADT-i-ness, etc for now, assuming kind :: Type
-- we want to derive Eq
mkDataDec :: Name -> [Con] -> Dec
mkDataDec name constructors = DataD [] name Nothing constructors defaultDerivClause


-- Generate a data declaration from a TestSum recipe, stash it in testSumDecs
mkTestDataDec :: TestSum -> DecState ()
mkTestDataDec tsum = getIndexPlan >>= \plan -> mkTestDataDecWithPlan plan tsum

mkTestDataDecWithPlan :: IndexPlan -> TestSum -> DecState ()
mkTestDataDecWithPlan plan tsum = do
  modify $ \(TestState ds g hcis is) -> TestState ds g mempty
  let mkHasIndices = \nm cs -> mkHasIndicesWithPlan plan nm cs
  case tsum of

    -- We treat this as a recipe for a data type with one constructor and no args, e.g.
    -- data TYPE034 = TYPE034
    Nullary -> do
      name        <- lift $ newName "NULLARY"
      let constrs = [NormalC name []]
      hcInstance  <- mkHasIndices name constrs
      dec         <- mkDataDec name constrs
      modify $ \ts -> ts {testSumDecs = (M.insert name dec ds),
                          hcInstances = hcInstance:hcis}

    Bool b -> do
      name        <- lift $ newName "Bool"
      let constrs = [NormalC name [mkBangType . ]]
      hcInstance  <- mkHasIndices name constrs
