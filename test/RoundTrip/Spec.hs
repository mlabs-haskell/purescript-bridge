{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module RoundTrip.Spec (roundtripSpec, stupidTest) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (guard, unless)
import Data.Aeson (eitherDecode, encode)
import Data.ByteString.Lazy.UTF8 (fromString, toString)
import Data.List (isInfixOf)
import Language.PureScript.Bridge (
  BridgePart,
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
import PlutusTx.LedgerTypes (writePlutusTypes)
import RoundTrip.Types (
  ANewtype,
  ANewtypeRec,
  ARecord,
  ASum,
  MyUnit,
  Request (ReqParseJson),
  Response (RespParseJson, RespParsePlutusData),
  TestData,
  TestEnum,
  TestMultiInlineRecords,
  TestNewtype,
  TestNewtypeRecord,
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

  (hin, hout, herr, hproc) <- runIO startPurescript
  describe "Round trip tests (Purescript <-> Haskell)" do
    it "should have a Purescript process running" $ do
      mayPid <- getPid hproc
      maybe
        (assertFailure "No process running")
        (\_ -> return ())
        mayPid

    it "should produce aeson-compatible argonaut instances" $ do
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
  where
    response js _ (RespParseJson payload) = js payload
    response _ pd (RespParsePlutusData payload) = pd payload

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

    startPurescript = do
      withCurrentDirectory "test/RoundTrip/app" do
        generateBridgedFiles
        spagoRun

    generateBridgedFiles = do
      writePSTypesWith
        defaultSwitch
        "src"
        (buildBridge myBridge)
        (myTypes <> myPlutusTypes)

myBridge :: BridgePart
myBridge = defaultBridge

myTypes :: [SumType 'Haskell]
myTypes =
  [ equal . genericShow . order . argonaut $ mkSumType @TestData
  , equal . genericShow . order . argonaut $ mkSumType @TestSum
  , equal . genericShow . order . argonaut $ mkSumType @TestRecursiveA
  , equal . genericShow . order . argonaut $ mkSumType @TestRecursiveB
  , functor . equal . genericShow . order . argonaut $ mkSumType @(TestRecord A)
  , equal . genericShow . order . argonaut $ mkSumType @TestNewtype
  , equal . genericShow . order . argonaut $ mkSumType @TestNewtypeRecord
  , equal . genericShow . order . argonaut $ mkSumType @TestMultiInlineRecords
  , equal . genericShow . order . argonaut $ mkSumType @TestTwoFields
  , equal . genericShow . order . argonaut $ mkSumType @TestEnum
  , equal . genericShow . order . argonaut $ mkSumType @MyUnit
  , equal . genericShow . order . argonaut $ mkSumType @Request
  , equal . genericShow . order . argonaut $ mkSumType @Response
  ]

myPlutusTypes :: [SumType 'Haskell]
myPlutusTypes =
  [ equal . genericShow . argonaut $ mkPlutusNewtype @ANewtype
  , equal . genericShow . argonaut $ mkPlutusNewtype @ANewtypeRec
  , equal . genericShow . argonaut $ unsafeMkPlutusDataType @ARecord
  , equal . genericShow . argonaut $ unsafeMkPlutusDataType @ASum
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
  (hin, hout, herr, hproc) <- runInteractiveCommand "spago run"
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
