module Main
  ( main
  ) where

import Aeson (decodeJsonString, encodeAeson, stringifyAeson)
import Data.Argonaut (JsonDecodeError, printJsonDecodeError)
import Data.Either (either, Either(Left, Right))
import Data.Maybe (maybe, Maybe)
import Deserialization.FromBytes (fromBytes', FromBytesError)
import Deserialization.PlutusData as DeserPd
import Effect (Effect)
import Effect.Class.Console (error, log)
import Error (E)
import FromData (fromData)
import Node.ReadLine (createConsoleInterface, noCompletion, question)
import Prelude (Unit, bind, discard, pure, show, (#), ($), (<<<), (<>))
import RoundTrip.Types
  ( TestData
  , TestPlutusData
  , Request(..)
  , Response(..)
  , RepType(..)
  )
import Serialization (toBytes)
import Serialization.PlutusData as SerPd
import Serialization.Types (PlutusData)
import ToData (toData)
import Type.Row (type (+))
import Types.ByteArray (hexToByteArray, byteArrayToHex)
import Untagged.Union (asOneOf)

main :: Effect Unit
main = do
  interface <- createConsoleInterface noCompletion
  log "I was born ready"
  go interface
  where
  go interface =
    interface # question "" \input -> do
      let
        reqOrErr :: Either JsonDecodeError Request
        reqOrErr = decodeJsonString input
      case reqOrErr of
        Left err -> do
          error $ "ps> Wanted Request got error: " <> printJsonDecodeError err
            <> " on input: "
            <> input
          log ""
        Right req -> do
          either
            ( \err -> do
                error err
                log $ stringifyAeson $ encodeAeson $ RespError err
            )
            ( \resp -> do
                error ""
                log $ stringifyAeson $ encodeAeson $ resp
            )
            (handleReq req)
      go interface

handleReq :: Request -> Either String Response
handleReq (Req RTJson str) = do
  testData <- either
    ( \err -> Left $ "ps> Wanted Json got err: " <> printJsonDecodeError err
        <> " on input: "
        <> str
    )
    pure
    (decodeJsonString str :: Either JsonDecodeError TestData)
  let payload = stringifyAeson $ encodeAeson testData
  pure $ RespSuccess RTJson payload
handleReq (Req RTPlutusData hex) = do
  -- Base16 -> ByteArray -> Cbor-> Foreign PlutusData -> CTL PlutusData -> TestPlutusData
  cbor <- maybe
    (Left $ "ps> Wanted base16 string got error on input: " <> hex)
    pure
    (hexToByteArray hex)
  pdF <- either
    ( \err -> Left $ "ps> Wanted Foreign PlutusData got error: " <> show err
        <> "on input: "
        <> show cbor
    )
    pure
    (fromBytes' cbor :: E (FromBytesError + ()) PlutusData)
  pdN <- maybe
    (Left "ps> Wanted Native PlutusData got error")
    pure
    (DeserPd.convertPlutusData pdF)
  testData <- maybe
    (Left $ "ps> Wanted TestData got error on input: " <> show pdN)
    pure
    (fromData pdN :: Maybe TestPlutusData)
  -- TestPlutusData -> CTL PlutusData -> Foreign PlutusData -> Cbor -> ByteArray -> Base16
  let pdN' = toData testData
  pdF' <- maybe
    (Left $ "ps> Wanted Foreign PlutusData got error on input: " <> show pdN')
    pure
    (SerPd.convertPlutusData pdN')
  let payload = (byteArrayToHex <<< toBytes <<< asOneOf) pdF'
  pure $ RespSuccess RTPlutusData payload
