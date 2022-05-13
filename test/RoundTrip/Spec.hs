{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
-- {-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-missing-import-lists #-}

module RoundTrip.Spec (roundtripSpec, stupidTest) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Exception (bracket)
import Control.Monad
import Data.Aeson (eitherDecode, encode)
import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Lazy.UTF8 (fromString, toString)
import Data.List (isInfixOf)
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
  mkPlutusNewtype,
  mkSumType,
  order,
  unsafeMkPlutusDataType,
  writePSTypesWith,
 )
import Language.PureScript.Bridge.TypeParameters (A)
import PlutusTx
import PlutusTx.LedgerTypes
import RoundTrip.Types (
  ANewtype,
  ANewtypeRec,
  ARecord,
  ASum,
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
import System.Directory (withCurrentDirectory)
import System.Exit (ExitCode (ExitSuccess))
import System.IO (BufferMode (LineBuffering), hGetLine, hPutStrLn, hSetBuffering)
import System.Process {-(
  readProcessWithExitCode,
  runInteractiveCommand,
  terminateProcess,
 )-}
import Test.HUnit (assertBool, assertEqual)
import Test.Hspec (Spec, around, aroundAll_, describe, it)
import Test.QuickCheck (arbitrary, generate)
import Test.QuickCheck.Property (Testable (property))

import           Control.Applicative      ((*>))
import           Control.Concurrent.Async (Concurrently (..))
import           Data.Conduit             (await, yield, (.|), runConduit, ConduitT)
import qualified  Data.Conduit.Combinators as CC
import qualified Data.Conduit.Binary      as CB
import qualified Data.Conduit.List        as CL
import           Data.Conduit.Process     (ClosedStream (..), streamingProcess,
                                           proc, waitForStreamingProcess)
import           System.IO                (stdin)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as B
import Control.Concurrent.Async
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
        (_, _, stderr) <- readProcessWithExitCode "spago" ["build"] ""
        assertBool stderr $ not $ "[warn]" `isInfixOf` stderr
      around withApp $
        it "should produce aeson-compatible argonaut instances" $
          \(hin, hout, herr, _) ->
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
        (myTypes <> myPlutusTypes)

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

  --aSum <- toString <$>  generate $ arbitrary @ASum

  --let input = toString $ encode @ASum aSum

  let myCreateProcess = (shell "spago run")

  ((toProcess,close),fromProcess,ClosedStream,cph) <- streamingProcess$  myCreateProcess {cwd = Just "./test/RoundTrip/app"}

  testCases <- replicateM 10 (BL.toStrict . encode @ASum <$> (generate $ arbitrary @ASum))

  input <- async $ (runConduit
            $ CL.sourceList testCases
           .| CC.iterM print
           .| toProcess) >> close

  output <- async $ runConduit $ fromProcess .| CC.iterM  (putStrLn . B.toString) .| CC.sinkNull


  void $ waitEither (input) output
  

  waitForStreamingProcess cph >>= print

{-
stupidTest' :: IO ()
stupidTest' = do
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
  --threadDelay 2000000
  --err <- hGetLine herr
  --unless (null err) $ putStrLn err

  putStrLn "\nSending ASum to PureScript...\n"
  let input = toString $ encode @ASum aSum
  hPutStrLn hin input
  --err' <- hGetLine herr
  --unless (null err') $ putStrLn err
  putStrLn "sent to purescript"
  getProcessExitCode hproc >>= \case
    Nothing -> do
      putStrLn "process still alive"
      output <- hGetLine hout
      putStrLn "\npurescript output:"
      putStrLn output
    Just _ -> putStrLn "process died :-("
-}
