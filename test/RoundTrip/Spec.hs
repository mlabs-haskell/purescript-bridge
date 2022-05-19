{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module RoundTrip.Spec (spec) where

-- (assertEqual, assertBool, assertFailure, Assertion)

import ArbitraryLedger (WEq ((@==)))
import Codec.Serialise qualified as Cbor
import Control.DeepSeq (deepseq)
import Control.Exception qualified as E
import Control.Monad (guard, unless)
import Data.Aeson (eitherDecode, encode)
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Lazy (fromStrict, toStrict)
import Data.ByteString.Lazy.UTF8 (fromString, toString)
import Data.CallStack (HasCallStack, SrcLoc, callStack)
import Data.Kind (Constraint, Type)
import Data.List (isInfixOf)
import Data.Text.Lazy qualified as T
import Language.PureScript.Bridge.SumType (
  SumType,
  argonaut,
  equal,
  functor,
  genericShow,
  mkPlutusDataType,
  mkPlutusNewtype,
  mkSumType,
  order,
 )
import Language.PureScript.Bridge.TypeInfo (Language (Haskell))
import Language.PureScript.Bridge.TypeParameters (A)
import PlutusTx (toData)
import PlutusTx.IsData.Class (fromData)
import PlutusTx.LedgerTypes (writeLedgerTypesAnd)
import RoundTrip.Types (
  ANewtype,
  ANewtypeRec,
  ARecord,
  ASum,
  MyUnit,
  RepType (RTJson, RTPlutusData),
  Request (Req),
  Response,
  TestData,
  TestEnum,
  TestMultiInlineRecords,
  TestNewtype,
  TestNewtypeRecord,
  TestPlutusData,
  TestRecord,
  TestRecursiveA,
  TestRecursiveB,
  TestSum,
  TestTwoFields,
  response,
 )
import System.Directory (createDirectoryIfMissing, withCurrentDirectory)
import System.Exit (ExitCode (ExitSuccess))
import System.IO (BufferMode (LineBuffering), hGetLine, hPutStrLn, hSetBuffering)
import System.Process (
  getPid,
  readProcessWithExitCode,
  runInteractiveCommand,
  terminateProcess,
 )
import Test.HUnit (Assertion, assertEqual, assertFailure)
import Test.HUnit.Lang (FailureReason (ExpectedButGot), HUnitFailure (HUnitFailure))
import Test.Hspec (Spec, beforeAll, describe, it)
import Test.QuickCheck.Property (Testable (property))
import Text.Pretty.Simple (pShow)

data PrinterFormat = Pretty | Ugly

class Equality (c :: Type -> Constraint) where
  equals :: forall (a :: Type). c a => a -> a -> Bool

instance Equality Eq where
  equals = (==)

instance Equality WEq where
  equals = (@==)

assertEqualWith ::
  forall (c :: Type -> Constraint) (a :: Type).
  (Equality c, c a, Show a) =>
  PrinterFormat ->
  String ->
  a ->
  a ->
  Assertion
assertEqualWith fmt preface expected actual =
  unless (equals @c actual expected) $ do
    prefaceMsg `deepseq` expectedMsg `deepseq` actualMsg `deepseq` E.throwIO (HUnitFailure location $ ExpectedButGot prefaceMsg expectedMsg actualMsg)
  where
    prefaceMsg
      | null preface = Nothing
      | otherwise = Just preface
    formatShow = \x -> case fmt of Pretty -> prettyShow x; Ugly -> show x
    expectedMsg = formatShow expected
    actualMsg = formatShow actual

prettyShow :: Show a => a -> String
prettyShow = T.unpack . pShow

location :: HasCallStack => Maybe SrcLoc
location = case reverse callStack of
  (_, loc) : _ -> Just loc
  [] -> Nothing

spec :: Spec
spec = describe "Round trip tests (Purescript <-> Haskell)" roundTripSpec

roundTripSpec :: Spec
roundTripSpec = do
  beforeAll (startPurescript $ rtProtoTypes <> myTypes <> myPlutusTypes) $
    describe "With plutus-ledger-api bridge" do
      it "should have a Purescript process running" $ \(_hin, _hout, _herr, hproc) -> do
        mayPid <- getPid hproc
        maybe
          (assertFailure "No process running")
          (\_ -> return ())
          mayPid
      it "should produce Aeson-compatible representations" $ \(hin, hout, herr, _hproc) -> do
        property $
          \testData ->
            do
              -- Prepare request
              let payload = toString $ encode @TestData testData
              -- IPC
              resp <- doReq hin herr hout (Req RTJson payload)
              -- Assert response
              jsonResp <-
                response
                  (\err -> assertFailure $ "hs> Wanted ResSuccess got ResError " <> err)
                  return
                  (\pd -> assertFailure $ "hs> Wanted RTJson got RTPlutusData: " <> pd)
                  resp
              assertEqualWith @Eq
                Pretty
                "hs> Round trip for payload should be ok"
                (Right testData)
                (eitherDecode @TestData (fromString jsonResp))
      it "should produce PlutusData compatible representations" $ \(hin, hout, herr, _hproc) -> do
        property $
          \testPlutusData ->
            do
              -- Prepare request
              let payload = encodeBase16 $ Cbor.serialise $ toData @TestPlutusData testPlutusData
              -- IPC
              resp <- doReq hin herr hout (Req RTPlutusData payload)
              -- Assert response
              pdResp <-
                response
                  (\err -> assertFailure $ "hs> Wanted ResSuccess got ResError " <> err)
                  (\json -> assertFailure $ "hs> Wanted RTPlutusData got RTJson " <> json)
                  return
                  resp
              cbor <-
                either
                  (\err -> assertFailure $ "hs> Wanted Base64 got error: " <> err)
                  return
                  (decodeBase16 pdResp)
              pd <-
                either
                  (\err -> assertFailure $ "hs> Wanted Cbor got error: " <> show err)
                  return
                  (Cbor.deserialiseOrFail cbor)
              assertEqualWith @WEq
                Pretty
                "hs> Round trip for payload should be ok"
                (Just testPlutusData)
                (fromData @TestPlutusData pd)
  where
    doReq hin herr hout req = do
      let jsonReq = toString $ encode @Request req
      -- putStrLn jsonReq -- DEBUG
      -- IPC
      hPutStrLn hin jsonReq
      err <- hGetLine herr
      assertEqual "hs> Purescript shouldn't report an error" "" err
      output <- hGetLine hout
      -- Assert response
      either
        (\err -> assertFailure $ "hs> Wanted Response got error: " <> err)
        return
        (eitherDecode @Response $ fromString output)

    encodeBase16 = toString . fromStrict . Base16.encode . toStrict
    decodeBase16 str = do
      bs <- Base16.decode $ toStrict . fromString $ str
      return $ fromStrict bs

    waitUntil pred fd = do
      l <- hGetLine fd
      putStrLn $ "hs > waitUntil> " <> l
      Control.Monad.unless (pred l) (waitUntil pred fd)

    spagoBuild = do
      (exitCode, _stdout, stderr) <- readProcessWithExitCode "spago" ["build"] ""
      guard $ exitCode == ExitSuccess
      guard $ not $ "[warn]" `isInfixOf` stderr
      guard $ "[info] Build succeeded." `isInfixOf` stderr

    spagoRun = do
      (hin, hout, herr, hproc) <- runInteractiveCommand "spago run"
      mapM_ (`hSetBuffering` LineBuffering) [hin, hout, herr]
      -- Wait until Spago is done with the build
      waitUntil (== "[info] Build succeeded.") herr
      -- Wait for initial "ready" log message
      waitUntil (== "I was born ready") hout
      pure (hin, hout, herr, hproc)

    _stopPurescript = terminateProcess -- TODO: Figure out `after` cleanup
    startPurescript types = do
      withCurrentDirectory "test/RoundTrip/app" do
        createDirectoryIfMissing True "generated"
        writeLedgerTypesAnd
          "generated"
          types
        spagoBuild
        spagoRun

rtProtoTypes :: [SumType 'Haskell]
rtProtoTypes =
  argonaut . equal . genericShow . order
    <$> [ mkSumType @Request
        , mkSumType @Response
        , mkSumType @RepType
        ]

myTypes :: [SumType 'Haskell]
myTypes =
  argonaut
    <$> [ equal . genericShow . order $ mkSumType @TestData
        , equal . genericShow . order $ mkSumType @TestSum
        , equal . genericShow . order $ mkSumType @TestRecursiveA
        , equal . genericShow . order $ mkSumType @TestRecursiveB
        , functor . equal . genericShow . order $ mkSumType @(TestRecord A)
        , equal . genericShow . order $ mkSumType @TestNewtype
        , equal . genericShow . order $ mkSumType @TestNewtypeRecord
        , equal . genericShow . order $ mkSumType @TestMultiInlineRecords
        , equal . genericShow . order $ mkSumType @TestTwoFields
        , equal . genericShow . order $ mkSumType @TestEnum
        , equal . genericShow . order $ mkSumType @MyUnit
        ]

myPlutusTypes :: [SumType 'Haskell]
myPlutusTypes =
  [ argonaut . equal . genericShow $ mkPlutusNewtype @ANewtype
  , argonaut . equal . genericShow $ mkPlutusNewtype @ANewtypeRec
  , argonaut . equal . genericShow $ mkPlutusDataType @ARecord
  , argonaut . equal . genericShow $ mkPlutusDataType @ASum
  , equal . genericShow $ mkPlutusDataType @TestPlutusData
  ]
