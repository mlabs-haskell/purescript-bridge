-- File auto generated by purescript-bridge! --
module TestData where

import Prelude

import ConstrIndices (class HasConstrIndices, fromConstr2Index)
import Control.Lazy (defer)
import Data.Argonaut.Core (jsonNull)
import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Argonaut.Decode.Aeson ((</$\>), (</*\>), (</\>), decode, null)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Argonaut.Encode.Aeson ((>$<), (>/\<), encode, null)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(Nothing, Just))
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(Tuple))
import Data.Tuple.Nested ((/\))
import FromData (class FromData, genericFromData)
import ToData (class ToData, genericToData)
import Data.Argonaut.Decode.Aeson as D
import Data.Argonaut.Encode.Aeson as E
import Data.Map as Map

data TwoRecords
  = FirstRecord
    { _fra :: String
    , _frb :: Int
    }
  | SecondRecord
    { _src :: Int
    , _srd :: Array Int
    }

instance EncodeJson TwoRecords where
  encodeJson = defer \_ -> case _ of
    FirstRecord {_fra, _frb} -> encodeJson
      { tag: "FirstRecord"
      , _fra: flip E.encode _fra E.value
      , _frb: flip E.encode _frb E.value
      }
    SecondRecord {_src, _srd} -> encodeJson
      { tag: "SecondRecord"
      , _src: flip E.encode _src E.value
      , _srd: flip E.encode _srd E.value
      }

instance DecodeJson TwoRecords where
  decodeJson = defer \_ -> D.decode
    $ D.sumType "TwoRecords" $ Map.fromFoldable
      [ "FirstRecord" /\ (FirstRecord <$> D.object "FirstRecord"
        { _fra: D.value :: _ String
        , _frb: D.value :: _ Int
        })
      , "SecondRecord" /\ (SecondRecord <$> D.object "SecondRecord"
        { _src: D.value :: _ Int
        , _srd: D.value :: _ (Array Int)
        })
      ]

derive instance Generic TwoRecords _

instance HasConstrIndices TwoRecords where
  constrIndices _ = fromConstr2Index [Tuple "FirstRecord" 0,Tuple "SecondRecord" 1]

instance ToData TwoRecords where
  toData x = genericToData x

instance FromData TwoRecords where
  fromData pd = genericFromData pd