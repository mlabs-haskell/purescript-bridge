module Main (main) where

import Control.Applicative (Alternative ((<|>)), Applicative ((<*)), (<**>))
import Control.Monad (void)
import Data.Semigroup ((<>))
import Options.Applicative (
  Applicative ((<*>)),
  Parser,
  ParserInfo,
  auto,
  command,
  customExecParser,
  flag',
  fullDesc,
  help,
  helper,
  info,
  long,
  metavar,
  option,
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
import PlutusTx.LedgerTypes (writeLedgerTypesAnd)
import Prelude (FilePath, IO, ($))

newtype Command
  = GeneratePlutusLedgerApiTypes GeneratePlutusLedgerApiTypesOpts

newtype GeneratePlutusLedgerApiTypesOpts = GeneratePlutusLedgerApiTypesOpts FilePath

gptOpts :: Parser GeneratePlutusLedgerApiTypesOpts
gptOpts =
  GeneratePlutusLedgerApiTypesOpts
    <$> strOption
      ( long "purs-dir"
          <> metavar "PURS_DIR"
          <> help "Directory to store the generated purescript type library"
      )

-- https://github.com/input-output-hk/plutus/tree/master/plutus-ledger-api
options :: Parser Command
options =
  subparser
    ( command
        "generate-plutus-ledger-api-types"
        ( info
            (GeneratePlutusLedgerApiTypes <$> gptOpts <* helper)
            (progDesc "Generate plutus-ledger-api Purescript types in a directory")
        )
    )

parserInfo :: ParserInfo Command
parserInfo = info (options <**> helper) (fullDesc <> progDesc "Plutus purescript-bridge cli tools")

main :: IO ()
main = do
  cmd <- customExecParser (prefs (showHelpOnEmpty <> showHelpOnError)) parserInfo
  case cmd of
    GeneratePlutusLedgerApiTypes (GeneratePlutusLedgerApiTypesOpts pursDir) -> writeLedgerTypesAnd pursDir []
