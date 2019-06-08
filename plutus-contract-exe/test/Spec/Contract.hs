{-# LANGUAGE TypeApplications #-}
module Spec.Contract(tests) where

import           Test.Tasty

import           Control.Monad                     (void)
import qualified Data.Aeson                        as Aeson
import           Examples.Game
import           Language.Plutus.Contract          as Con
import           Language.Plutus.Contract.Contract as Con
import qualified Language.Plutus.Contract.Event    as Event
import qualified Ledger.Ada                        as Ada
import           Prelude                           hiding (not)
import qualified Wallet.Emulator                   as EM

import           Spec.HUnit

tests :: TestTree
tests = testGroup "contracts"
  [ checkPredicate "slotGeq"
      (slotGeq 10)
      (waitingForSlot 10)
      $ void drain_

  , checkPredicate "selectEither"
      (selectEither (slotGeq 10) (slotGeq 5))
      (waitingForSlot 5)
      $ void drain_

  , checkPredicate "until"
      (slotGeq 10 `Con.until` 5)
      (waitingForSlot 5)
      $ void drain_

  , checkPredicate "both"
      (Con.both (slotGeq 10) (slotGeq 20))
      (waitingForSlot 10)
      $ void drain_

  , checkPredicate "fundsAtAddressGt"
      (fundsAtAddressGt gameAddress (Ada.adaValueOf 10))
      (interestingAddress gameAddress)
      $ void drain_

  , checkPredicate "watchAddressUntil"
      (watchAddressUntil gameAddress 5)
      (interestingAddress gameAddress <> waitingForSlot 5)
      $ void drain_

  , checkPredicate "endpoint"
      (endpoint @() "ep")
      (endpointAvailable "ep")
      $ void drain_

  , checkPredicate "call endpoint (1)"
      (endpoint @Int "1" >> endpoint @Int "2")
      (endpointAvailable "1")
      $ void drain_

  , checkPredicate "call endpoint (2)"
      (Con.offer
        (Event.endpoint "1" (Aeson.toJSON (1 :: Int)))
        (snd $ Con.drain $ endpoint @Int "1" >> endpoint @Int "2"))
      (endpointAvailable "2" <> not (endpointAvailable "1"))
      $ void drain_

  , checkPredicate "call endpoint (3)"
      (endpoint @Int "1" >> endpoint @Int "2")
      (endpointAvailable "2" <> not (endpointAvailable "1"))
      (callEndpoint w1 "1" (1::Int))
  ]

w1 :: EM.Wallet
w1 = EM.Wallet 1

