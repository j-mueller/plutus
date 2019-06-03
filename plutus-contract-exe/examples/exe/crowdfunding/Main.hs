{-# LANGUAGE ApplicativeDo      #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeOperators      #-}
-- | Contract interface for the crowdfunding contract
module Main where

import           Network.Wai.Handler.Warp         (run)

import           Examples.Crowdfunding            (crowdfunding)
import           Language.Plutus.Contract.Servant (contractApp)

main :: IO ()
main = run 8080 (contractApp crowdfunding)
