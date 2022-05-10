module Test.Main where

import Test.QuickCheck.Combinators (AtLeast (..), AtMost (..), Between (..))
import Test.QuickCheck (Result, (>=?), (<=?), withHelp, quickCheck)

import Prelude
import Data.Array as Array
import Data.Typelevel.Num (toInt', D6, D12)
import Type.Proxy (Proxy (..))
import Effect (Effect)
import Effect.Console (log)

main :: Effect Unit
main = do
  log "Starting tests..."
  log "  - is \"at least\" long"
  quickCheck isAtLeastLong
  log "  - is \"at most\" long"
  quickCheck isAtMostLong
  log "  - is \"between\" long"
  quickCheck isBetweenLong


isAtLeastLong :: AtLeast D6 Array Unit -> Result
isAtLeastLong (AtLeast xs) =
  let n = toInt' (Proxy :: Proxy D6)
  in  Array.length xs >=? n

isAtMostLong :: AtMost D12 Array Unit -> Result
isAtMostLong (AtMost xs) =
  let n = toInt' (Proxy :: Proxy D12)
  in  Array.length xs <=? n

isBetweenLong :: Between D6 D12 Array Unit -> Result
isBetweenLong (Between xs) =
  let n = toInt' (Proxy :: Proxy D6)
      m = toInt' (Proxy :: Proxy D12)
  in  withHelp (Array.length xs >= n && Array.length xs <= m)
        ("Not between lengths: " <> show n <> ", " <> show m <> ", xs: " <> show xs)
