{-# LANGUAGE ApplicativeDo      #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeOperators      #-}
-- | Contract interface for the guessing game
module Main where

import           Examples.Game                    (game)
import           Language.Plutus.Contract.Servant (contractApp)
import           Network.Wai.Handler.Warp         (run)

main :: IO ()
main = run 8080 (contractApp game)
