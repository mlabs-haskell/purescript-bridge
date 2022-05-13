{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module RoundTrip.Spec (roundtripSpec, stupidTest) where

import Codec.Serialise qualified as Cbor
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (guard, unless)
import Data.Aeson (eitherDecode, encode)
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Lazy (fromStrict, toStrict)
import Data.ByteString.Lazy.UTF8 (fromString, toString)
import Data.List (isInfixOf)
import Language.PureScript.Bridge (
  Language (Haskell),
  SumType,
  argonaut,
  buildBridge,
  defaultBridge,
  defaultSwitch,
  equal,
  functor,
  genericShow,
  mkPlutusNewtype,
  mkSumType,
  order,
  unsafeMkPlutusDataType,
  writePSTypesWith,
 )
import Language.PureScript.Bridge.TypeParameters (A)
import PlutusTx (toData)
import PlutusTx.IsData.Class (fromData)
import PlutusTx.LedgerTypes (plutusLedgerApiBridge, writePlutusTypes)
import RoundTrip.Types (
  ANewtype,
  ANewtypeRec,
  ARecord,
  ASum,
  MyUnit,
  Request (ReqParseJson, ReqParsePlutusData),
  Response (RespParseJson, RespParsePlutusData),
  TestData,
  TestEnum,
  TestMultiInlineRecords,
  TestNewtype,
  TestNewtypeRecord,
  TestPlutusData,
  TestPlutusDataSum,
  TestRecord,
  TestRecursiveA,
  TestRecursiveB,
  TestSum,
  TestTwoFields,
 )
import System.Directory (withCurrentDirectory)
import System.Exit (ExitCode (ExitSuccess))
import System.IO (BufferMode (LineBuffering), hGetLine, hPutStrLn, hSetBuffering)
import System.Process (
  getPid,
  readProcessWithExitCode,
  runInteractiveCommand,
  terminateProcess,
 )
import Test.HUnit (assertBool, assertEqual, assertFailure)
import Test.Hspec (Spec, describe, it, runIO)
import Test.QuickCheck (arbitrary, generate)
import Test.QuickCheck.Property (Testable (property))

roundtripSpec :: Spec
roundtripSpec = do
  describe
    "Round trip prerequisite tests"
    do
      it "`[test/RoundTrip/app] $ spago build` should work" do
        (exitCode, stdout, stderr) <- readProcessWithExitCode "spago" ["build"] ""
        assertEqual (stdout <> stderr) exitCode ExitSuccess
      it "`[test/RoundTrip/app] $ spago build` should not warn of unused packages buildable" do
        (_, _, stderr) <- readProcessWithExitCode "spago" ["build"] ""
        assertBool stderr $ not $ "[warn]" `isInfixOf` stderr

  (hin, hout, herr, hproc) <- runIO $ startPurescript defaultBridge myTypes
  describe "Round trip tests (Purescript <-> Haskell)" do
    it "should have a Purescript process running" $ do
      mayPid <- getPid hproc
      maybe
        (assertFailure "No process running")
        (\_ -> return ())
        mayPid

    it "should produce Aeson-compatible representations" $ do
      property $
        \testData ->
          do
            -- Prepare request
            let payload = toString $ encode @TestData testData
                req = toString $ encode @Request (ReqParseJson payload)
            -- IPC
            hPutStrLn hin req
            err <- hGetLine herr
            output <- hGetLine hout
            -- Assert response
            resp <-
              either
                (\err -> assertFailure $ "hs> Wanted Response got error: " <> err)
                return
                (eitherDecode @Response $ fromString output)
            jsonResp <-
              response
                return
                (\pd -> assertFailure $ "hs> Wanted RespParseJson got RespParsePlutusData: " <> pd)
                resp
            assertEqual "hs> Purescript shouldn't report an error" "" err
            assertEqual
              "hs> Round trip for payload should be ok"
              (Right testData)
              (eitherDecode @TestData (fromString jsonResp))
  runIO $ stopPurescript hproc
  (hin, hout, herr, hproc) <- runIO $ startPurescript plutusLedgerApiBridge myPlutusTypes
  describe
    "Round trip tests (Purescript <-> Haskell) with ledger bridge"
    do
      it "should have a Purescript process running" $ do
        mayPid <- getPid hproc
        maybe
          (assertFailure "No process running")
          (\_ -> return ())
          mayPid
      it "should produce PlutusData compatible representations" $ do
        property $
          \testPlutusData ->
            do
              -- Prepare request
              let payload = encodeBase16 $ Cbor.serialise $ toData @TestPlutusData testPlutusData
                  req = toString $ encode @Request (ReqParsePlutusData payload)
              -- IPC
              hPutStrLn hin req
              err <- hGetLine herr
              output <- hGetLine hout
              -- Assert response
              resp <-
                either
                  (\err -> assertFailure $ "hs> Wanted Response got error: " <> err)
                  return
                  (eitherDecode @Response $ fromString output)
              pdResp <-
                response
                  return
                  (\json -> assertFailure $ "hs> Wanted RespParsePlutusData got RespParseJson: " <> json)
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
              assertEqual "hs> Purescript shouldn't report an error" "" err
              assertEqual
                "hs> Round trip for payload should be ok"
                (Just testPlutusData)
                (fromData @TestPlutusData pd)
  runIO $ stopPurescript hproc
  where
    response js _ (RespParseJson payload) = js payload
    response _ pd (RespParsePlutusData payload) = pd payload

    encodeBase16 = toString . fromStrict . Base16.encode . toStrict
    decodeBase16 str = do
      bs <- Base16.decode $ toStrict . fromString $ str
      return $ fromStrict bs

    spagoRun = do
      (hin, hout, herr, hproc) <- runInteractiveCommand "spago run"
      mapM_ (`hSetBuffering` LineBuffering) [hin, hout, herr]
      -- Wait until Spago is done with the build
      let waitUntilBuildSucceded = do
            l <- hGetLine herr
            Control.Monad.unless (l == "[info] Build succeeded.") waitUntilBuildSucceded
      waitUntilBuildSucceded
      -- Wait for initial "ready" log message
      l <- hGetLine hout
      guard $ l == "ready"
      pure (hin, hout, herr, hproc)

    stopPurescript = terminateProcess

    startPurescript bridge types = do
      withCurrentDirectory "test/RoundTrip/app" do
        generateBridgedFiles bridge types
        spagoRun

    generateBridgedFiles bridge types = do
      writePSTypesWith
        defaultSwitch
        "generated"
        (buildBridge bridge)
        types

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
        , equal . genericShow . order $ mkSumType @Request
        , equal . genericShow . order $ mkSumType @Response
        , equal . genericShow . order $ mkSumType @TestPlutusData
        , equal . genericShow . order $ mkSumType @TestPlutusDataSum
        ]

myPlutusTypes :: [SumType 'Haskell]
myPlutusTypes =
  [ equal . genericShow $ mkPlutusNewtype @ANewtype
  , equal . genericShow $ mkPlutusNewtype @ANewtypeRec
  , equal . genericShow $ unsafeMkPlutusDataType @ARecord
  , equal . genericShow $ unsafeMkPlutusDataType @ASum
  , equal . genericShow . order $ unsafeMkPlutusDataType @TestPlutusData
  , equal . genericShow . order $ unsafeMkPlutusDataType @TestPlutusDataSum
  ]

stupidTest :: IO ()
stupidTest = do
  putStrLn "writing plutus types...\n"
  withCurrentDirectory "test/RoundTrip/app" $ writePlutusTypes "src" myPlutusTypes

  aSum <- generate $ arbitrary @ASum

  let input = toString $ encode @ASum aSum

  putStrLn "Generated ASum:\n"
  print aSum
  putStrLn "\nPlutus Data:"
  print (toData aSum)
  putStrLn "\nJSON:"
  putStrLn input

  putStrLn "\nstarting PureScript process..."
  (hin, hout, herr, _) <- runInteractiveCommand "spago run"
  mapM_ (`hSetBuffering` LineBuffering) [hin, hout, herr]
  threadDelay 2000000
  --err <- hGetLine herr
  --unless (null err) $ putStrLn err

  forkIO $ putStrLn "\nSending ASum to PureScript...\n"
  let input = toString $ encode @ASum aSum
  forkIO $ hPutStrLn hin input
  --err' <- hGetLine herr
  --unless (null err') $ putStrLn err

  output <- hGetLine hout
  putStrLn "\npurescript output:"
  putStrLn output
