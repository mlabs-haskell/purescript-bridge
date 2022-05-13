module Main
  ( main
  ) where

import Prelude (Unit, bind, discard, pure, show, (#), ($), (<<<), (<>), (=<<))
import Data.Argonaut.Core (stringify)
import Data.Argonaut.Decode
  ( JsonDecodeError
  , decodeJson
  , parseJson
  , printJsonDecodeError
  )
import Data.Argonaut.Encode (encodeJson)
import Data.Either (either, Either(Left, Right))
import Effect (Effect)
import Effect.Class.Console (error, log)
import Node.ReadLine (createConsoleInterface, noCompletion, question)
import RoundTrip.Types
  ( TestData
  , TestPlutusData
  , Request(..)
  , Response(..)
  , RepType(..)
  )
import ToData (toData)
import FromData (fromData)
import Deserialization.FromBytes (fromBytes', FromBytesError)
import Error (E)
import Data.Maybe (maybe, Maybe)
import Serialization.Types
  ( PlutusData
  )
import Type.Row (type (+))
import Deserialization.PlutusData as DeserPd
import Serialization.PlutusData as SerPd
import Types.ByteArray (byteArrayFromAscii, byteArrayToHex)
import Serialization (toBytes)
import Untagged.Union (asOneOf)

main :: Effect Unit
main = do
  interface <- createConsoleInterface noCompletion
  log "ready"
  go interface
  where
  go interface =
    interface # question "" \input -> do
      let
        reqOrErr :: Either JsonDecodeError Request
        reqOrErr = decodeJson =<< parseJson input
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
                log $ stringify $ encodeJson $ RespError err
            )
            ( \resp -> do
                error ""
                log $ stringify $ encodeJson $ resp
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
    (decodeJson =<< parseJson str :: Either JsonDecodeError TestData)
  let payload = stringify $ encodeJson testData
  pure $ RespSuccess RTJson payload
handleReq (Req RTPlutusData ascii) = do
  -- Base16 + Cbor (ascii) -> Foreign PlutusData -> CTL PlutusData -> TestPlutusData
  cbor <- maybe
    (Left $ "ps> Wanted base16 string got error on input: " <> ascii)
    pure
    (byteArrayFromAscii ascii)
  pdF <- either
    ( \err -> Left $ "ps> Wanted Foreign PlutusData got error: " <> show err
        <> "on input: "
        <> show cbor
    )
    pure
    (fromBytes' cbor :: E (FromBytesError + ()) PlutusData)
  pdN <- maybe
    (Left $ "ps> Wanted Native PlutusData got error on input: " <> show cbor)
    pure
    (DeserPd.convertPlutusData pdF)
  testData <- maybe
    (Left $ "ps> Wanted TestData got error on input: " <> ascii)
    pure
    (fromData pdN :: Maybe TestPlutusData)
  -- TestPlutusData -> CTL PlutusData -> Foreign PlutusData -> Base16 + Cbor ascii
  let pdN' = toData testData
  pdF' <- maybe
    (Left $ "ps> Wanted Foreign PlutusData got error on input: " <> show cbor)
    pure
    (SerPd.convertPlutusData pdN')
  let payload = (byteArrayToHex <<< toBytes <<< asOneOf) pdF'
  pure $ RespSuccess RTPlutusData payload
