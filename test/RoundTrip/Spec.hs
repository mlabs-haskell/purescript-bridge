{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module RoundTrip.Spec (roundtripSpec) where

import Control.Exception (bracket)
import Control.Monad (guard, unless)
import Data.Aeson (FromJSON, ToJSON (toJSON), eitherDecode, encode, fromJSON)
import Data.ByteString.Lazy (hGetContents, hPutStr, putStr, stripSuffix)
import Data.ByteString.Lazy.UTF8 (fromString, toString)
import Data.Foldable (traverse_)
import Data.List (isInfixOf)
import Data.Maybe (fromJust, fromMaybe, isJust, isNothing)
import Data.Proxy (Proxy (Proxy))
import GHC.Generics (Generic)
import Language.PureScript.Bridge (
  BridgePart,
  Language (..),
  SumType,
  argonaut,
  buildBridge,
  defaultBridge,
  defaultSwitch,
  equal,
  functor,
  genericShow,
  mkSumType,
  order,
  writePSTypes,
  writePSTypesWith,
 )
import Language.PureScript.Bridge.TypeParameters (A)
import RoundTrip.Types (
  MyUnit,
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
import System.Directory (removeDirectoryRecursive, removeFile, withCurrentDirectory)
import System.Exit (ExitCode (ExitSuccess))
import System.IO (BufferMode (LineBuffering), Handle, hFlush, hGetLine, hPutStrLn, hSetBuffering, stderr, stdout)
import System.Process (
  CreateProcess (std_err, std_in, std_out),
  StdStream (CreatePipe),
  createProcess,
  getProcessExitCode,
  proc,
  readProcessWithExitCode,
  runInteractiveCommand,
  runInteractiveProcess,
  terminateProcess,
  waitForProcess,
 )
import Test.HUnit (assertBool, assertEqual)
import Test.Hspec (Spec, around, aroundAll_, around_, describe, it)
import Test.Hspec.Expectations.Pretty (shouldBe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (noShrinking, once, verbose, withMaxSuccess)
import Test.QuickCheck.Property (Testable (property))

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
  ]

roundtripSpec :: Spec
roundtripSpec = do
  aroundAll_ withProject $
    describe "writePSTypesWith" do
      it "should be buildable" do
        (exitCode, stdout, stderr) <- readProcessWithExitCode "spago" ["build"] ""
        assertEqual (stdout <> stderr) exitCode ExitSuccess
      it "should not warn of unused packages buildable" do
        (exitCode, stdout, stderr) <- readProcessWithExitCode "spago" ["build"] ""
        assertBool stderr $ not $ "[warn]" `isInfixOf` stderr
      around withApp $
        it "should produce aeson-compatible argonaut instances" $
          \(hin, hout, herr, hproc) ->
            property $
              \testData -> do
                let input = toString $ encode @TestData testData
                hPutStrLn hin input
                err <- hGetLine herr
                output <- hGetLine hout
                assertEqual input "" err
                assertEqual output (Right testData) $ eitherDecode @TestData $ fromString output
  where
    withApp = bracket runApp killApp
    runApp = do
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

    killApp (_, _, _, hproc) = terminateProcess hproc

    withProject runSpec =
      withCurrentDirectory "test/RoundTrip/app" $ generate *> runSpec

    generate = do
      writePSTypesWith
        defaultSwitch
        "src"
        (buildBridge myBridge)
        myTypes
