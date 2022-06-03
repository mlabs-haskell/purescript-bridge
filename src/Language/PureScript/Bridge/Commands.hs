module Language.PureScript.Bridge.Commands (mainWith) where

import Control.Applicative (Applicative ((<*)), (<**>))
import Data.Semigroup ((<>))
import Options.Applicative (
  Parser,
  ParserInfo,
  command,
  customExecParser,
  fullDesc,
  help,
  helper,
  info,
  long,
  metavar,
  prefs,
  progDesc,
  showDefault,
  showHelpOnEmpty,
  showHelpOnError,
  strOption,
  subparser,
  value,
  (<$>),
 )
import System.Directory (createDirectoryIfMissing)
import Prelude (Bool (True), FilePath, IO, String)

newtype Command
  = GenerateTypes GenerateTypesOpts

newtype GenerateTypesOpts = GenerateTypesOpts FilePath

gptOpts :: Parser GenerateTypesOpts
gptOpts =
  GenerateTypesOpts
    <$> strOption
      ( long "generated-dir"
          <> metavar "GENERATED_DIR"
          <> help "Directory to store the generated Purescript type library"
          <> value "generated"
          <> showDefault
      )

options :: Parser Command
options =
  subparser
    ( command
        "generate-types"
        ( info
            (GenerateTypes <$> gptOpts <* helper)
            (progDesc "Generate Purescript types in a directory")
        )
    )

parserInfo :: String -> ParserInfo Command
parserInfo desc = info (options <**> helper) (fullDesc <> progDesc desc)

mainWith :: String -> (FilePath -> IO ()) -> IO ()
mainWith desc act = do
  cmd <- customExecParser (prefs (showHelpOnEmpty <> showHelpOnError)) (parserInfo desc)
  case cmd of
    GenerateTypes (GenerateTypesOpts pursDir) -> do
      createDirectoryIfMissing True pursDir
      act pursDir
