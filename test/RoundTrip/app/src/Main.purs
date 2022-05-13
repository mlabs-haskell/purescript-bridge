module Main
  ( main
  ) where

import Prelude (Unit, bind, discard, pure, unit, (#), ($), (<>), (=<<))
import Data.Argonaut.Core (stringify)
import Data.Argonaut.Decode
  ( JsonDecodeError
  , decodeJson
  , parseJson
  , printJsonDecodeError
  )
import Data.Argonaut.Encode (encodeJson)
import Data.Either (Either(Left, Right))
import Effect (Effect)
import Effect.Class.Console (error, log)
import Node.ReadLine (createConsoleInterface, noCompletion, question)
import RoundTrip.Types (TestData, Request(..), Response(..))
-- import ToData (toData)
-- import FromData (fromData)
import Data.BigInt

-- import Data.Maybe (Maybe(Nothing, Just))

main :: Effect Unit
main = do
  interface <- createConsoleInterface noCompletion
  log "ready"
  go interface
  where
  go interface =
    interface # question "" \input -> do
      let
        req :: Either JsonDecodeError Request
        req = decodeJson =<< parseJson input
      case req of
        Left err -> do
          error $ "ps> Wanted Request got error: " <> printJsonDecodeError err
            <> " on input: "
            <> input
          log ""
        Right req' -> handleReq req'
      go interface

handleReq :: Request -> Effect Unit
handleReq (ReqParseJson json) = do
  let
    testDataOrErr =
      decodeJson =<< parseJson json :: Either JsonDecodeError TestData
  case testDataOrErr of
    Left err -> do
      error $ "ps> Wanted TestData got error: " <> printJsonDecodeError err
        <> " on input: "
        <> json
      log ""
    Right testData -> do
      error ""
      let payload = stringify $ encodeJson testData
      log $ stringify $ encodeJson (RespParseJson payload)
handleReq (ReqParsePlutusData _) = pure unit
-- case (fromData pd) of
--   Nothing -> do
--         error $ "ps> json parsing errored on: " <> pd
--         log ""
--   Just testData -> do
--     error ""
--     log $ toData testData
