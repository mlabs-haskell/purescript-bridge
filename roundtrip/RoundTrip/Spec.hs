{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Spec (main) where

import ArbitraryLedger (WEq ((@==)))
import Codec.Serialise qualified as Cbor
import Control.DeepSeq (deepseq)
import Control.Exception qualified as E
import Control.Monad (unless)
import Data.Aeson (eitherDecode, encode)
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Lazy (fromStrict, toStrict)
import Data.ByteString.Lazy.UTF8 (fromString, toString)
import Data.CallStack (HasCallStack, SrcLoc, callStack)
import Data.Kind (Constraint, Type)
import Data.Text.Lazy qualified as T
import PlutusTx (toData)
import PlutusTx.IsData.Class (fromData)
import System.IO (BufferMode (LineBuffering), hGetLine, hPutStrLn, hSetBuffering)
import System.Process (
  getPid,
  runInteractiveCommand,
  terminateProcess,
 )
import Test.HUnit (Assertion, assertEqual, assertFailure)
import Test.HUnit.Lang (FailureReason (ExpectedButGot), HUnitFailure (HUnitFailure))
import Test.Hspec (Spec, beforeAll, describe, hspec, it)
import Test.QuickCheck.Property (Testable (property))
import Text.Pretty.Simple (pShow)
import Types (
  RepType (RTJson, RTPlutusData),
  Request (Req),
  Response,
  TestData,
  TestPlutusData,
  response,
 )

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

roundTripSpec :: Spec
roundTripSpec = do
  beforeAll startPurescript $
    describe "With plutus-ledger-api bridge" do
      it "should have a Purescript process running" $ \(_hin, _hout, _herr, hproc) -> do
        mayPid <- getPid hproc
        maybe
          (assertFailure "No process running")
          (\_ -> return ())
          mayPid
      it "should be Aeson compatible" $ \(hin, hout, herr, _hproc) -> do
        property $
          \testData ->
            do
              -- Prepare request
              let payload = toString $ encode @TestData testData
              -- IPC
              resp <- doReq hin herr hout (Req RTJson payload)
              -- Assert response
              payload' <-
                response
                  (\err -> assertFailure $ "hs> Wanted ResSuccess got ResError " <> err)
                  return
                  (\pd -> assertFailure $ "hs> Wanted RTJson got RTPlutusData: " <> pd)
                  resp
              assertEqualWith @Eq
                Pretty
                "hs> Round trip for payload should be ok"
                (Right testData)
                (eitherDecode @TestData (fromString payload'))
      it "should be PlutusData and Cbor compatible" $ \(hin, hout, herr, _hproc) -> do
        property $
          \testPlutusData ->
            do
              -- Prepare request
              let payload = encodeBase16 $ Cbor.serialise $ toData @TestPlutusData testPlutusData
              -- IPC
              resp <- doReq hin herr hout (Req RTPlutusData payload)
              -- Assert response
              payload' <-
                response
                  (\err -> assertFailure $ "hs> Wanted ResSuccess got ResError " <> err)
                  (\json -> assertFailure $ "hs> Wanted RTPlutusData got RTJson " <> json)
                  return
                  resp
              assertEqualWith @Eq Pretty "Cbor byte encodings should match" payload payload'
              cbor <-
                either
                  (\err -> assertFailure $ "hs> Wanted Base64 got error: " <> err)
                  return
                  (decodeBase16 payload')
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
      putStrLn $ "hs> " <> show req -- DEBUG
      -- IPC
      hPutStrLn hin jsonReq
      err <- hGetLine herr
      assertEqual "hs> Purescript shouldn't report an error" "" err
      output <- hGetLine hout
      -- Assert response
      resp <-
        either
          (\err -> assertFailure $ "hs> Wanted Response got error: " <> err)
          return
          (eitherDecode @Response $ fromString output)
      putStrLn $ "ps> " <> show resp -- DEBUG
      return resp

    encodeBase16 = toString . fromStrict . Base16.encode . toStrict
    decodeBase16 str = do
      bs <- Base16.decode $ toStrict . fromString $ str
      return $ fromStrict bs

    waitUntil pred fd = do
      l <- hGetLine fd
      putStrLn $ "hs> waitUntil> " <> l
      Control.Monad.unless (pred l) (waitUntil pred fd)

    startPurescript = do
      (hin, hout, herr, hproc) <- runInteractiveCommand "roundtrip-test-run-with-node"
      mapM_ (`hSetBuffering` LineBuffering) [hin, hout, herr]
      -- Wait for initial "ready" log message
      waitUntil (== "I was born ready") hout
      pure (hin, hout, herr, hproc)

    _stopPurescript = terminateProcess -- TODO: Figure out `after` cleanup

main :: IO ()
main = hspec roundTripSpec
