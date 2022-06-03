{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Language.PureScript.Bridge.Commands (mainWith)
import Language.PureScript.Bridge.SumType (SumType, argonaut, equal, functor, genericShow, mkPlutusDataType, mkPlutusNewtype, mkSumType, order)
import Language.PureScript.Bridge.TypeInfo (Language (Haskell))
import Language.PureScript.Bridge.TypeParameters (A)
import PlutusTx.LedgerTypes (writeLedgerTypesAnd)
import Types (ANewtype, ANewtypeRec, ARecord, ASum, MyUnit, RepType, Request, Response, TestData, TestEnum, TestMultiInlineRecords, TestNewtype, TestNewtypeRecord, TestPlutusData, TestRecord, TestRecursiveA, TestRecursiveB, TestSum, TestTwoFields, TypesWithMap)

main :: IO ()
main = mainWith "Cli to generate types for the RoundTrip test" $ \pursDir -> do
  writeLedgerTypesAnd
    pursDir
    (rtProtoTypes <> myTypes <> typesWithPlutusTypes)

rtProtoTypes :: [SumType 'Haskell]
rtProtoTypes =
  argonaut . equal . genericShow . order
    <$> [ mkSumType @Request
        , mkSumType @Response
        , mkSumType @RepType
        ]

myTypes :: [SumType 'Haskell]
myTypes =
  argonaut . equal . genericShow
    <$> [mkSumType @TestData]
      <> ( order
            <$> [ mkSumType @TestSum
                , mkSumType @TestRecursiveA
                , mkSumType @TestRecursiveB
                , functor $ mkSumType @(TestRecord A)
                , mkSumType @TestNewtype
                , mkSumType @TestNewtypeRecord
                , mkSumType @TestMultiInlineRecords
                , mkSumType @TestTwoFields
                , mkSumType @TestEnum
                , mkSumType @MyUnit
                ]
         )

typesWithPlutusTypes :: [SumType 'Haskell]
typesWithPlutusTypes =
  argonaut . equal . genericShow
    <$> [ mkPlutusNewtype @ANewtype
        , mkPlutusNewtype @ANewtypeRec
        , mkPlutusDataType @ARecord
        , mkPlutusDataType @ASum
        , mkPlutusDataType @TypesWithMap
        , mkPlutusDataType @TestPlutusData
        ]
