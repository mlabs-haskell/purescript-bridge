{-# LANGUAGE DataKinds #-}

module Language.PureScript.Bridge.Tuple (step, isTuple, TupleParserState, tupleBridge) where

import Data.Text qualified as T
import Language.PureScript.Bridge.Builder (BridgePart, doCheck)
import Language.PureScript.Bridge.PSTypes (psTuple)
import Language.PureScript.Bridge.TypeInfo (
  HasHaskType (haskType),
  HaskellType,
  TypeInfo (_typeName),
 )

tupleBridge :: BridgePart
tupleBridge = doCheck haskType isTuple >> psTuple

data TupleParserState
  = Start
  | OpenFound
  | ColonFound
  | Tuple
  | NoTuple
  deriving stock (Eq, Show)

step :: TupleParserState -> Char -> TupleParserState
step Start '(' = OpenFound
step Start _ = NoTuple
step OpenFound ',' = ColonFound
step OpenFound _ = NoTuple
step ColonFound ',' = ColonFound
step ColonFound ')' = Tuple
step ColonFound _ = NoTuple
step Tuple _ = NoTuple
step NoTuple _ = NoTuple

isTuple :: HaskellType -> Bool
isTuple = (== Tuple) . T.foldl' step Start . _typeName
