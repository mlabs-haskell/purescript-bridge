module Main (main) where

import Language.PureScript.Bridge.Commands (mainWith)
import PlutusTx.LedgerTypes (writeLedgerTypesAnd)
import Prelude (IO, ($))

main :: IO ()
main = mainWith "Cardano/Plutus purescript-bridge cli tools" $ \pursDir -> writeLedgerTypesAnd pursDir []
