{-# LANGUAGE TypeApplications #-}
module Spec.Contract(tests) where

import           Test.Tasty

import           Examples.Game
import           Language.Plutus.Contract       as Con
import qualified Language.Plutus.Contract.Event as Event
import qualified Ledger.Ada                     as Ada
import           Prelude                        hiding (not)
import qualified Wallet.Emulator                as EM

import           Spec.HUnit

tests :: TestTree
tests = testGroup "contracts"
    [ checkPredicate "slotGeq"
        (slotGeq 10)
        (waitingForSlot 10)
        $ pure ()

    , checkPredicate "selectEither"
        (selectEither (slotGeq 10) (slotGeq 5))
        (waitingForSlot 5)
        $ pure ()

    , checkPredicate "until"
        (slotGeq 10 `Con.until` 5)
        (waitingForSlot 5)
        $ pure ()

    , checkPredicate "both"
        (Con.both (slotGeq 10) (slotGeq 20))
        (waitingForSlot 10)
        $ pure ()

    , checkPredicate "both (2)"
        (Con.both (slotGeq 10) (slotGeq 20))
        (waitingForSlot 20)
        $ event_ (Event.changeSlot 10)

    , checkPredicate "fundsAtAddressGt"
        (fundsAtAddressGt gameAddress (Ada.adaValueOf 10))
        (interestingAddress gameAddress)
        $ pure ()

    , checkPredicate "watchAddressUntil"
        (watchAddressUntil gameAddress 5)
        (interestingAddress gameAddress <> waitingForSlot 5)
        $ pure ()

    , checkPredicate "endpoint"
        (endpoint @() "ep")
        (endpointAvailable "ep")
        $ pure ()

    , checkPredicate "call endpoint (1)"
        (endpoint @Int "1" >> endpoint @Int "2")
        (endpointAvailable "1")
        $ pure ()

    , checkPredicate "call endpoint (2)"
        (endpoint @Int "1" >> endpoint @Int "2")
          (endpointAvailable "2" <> not (endpointAvailable "1"))
        (callEndpoint w1 "1" (1::Int))
    ]

w1 :: EM.Wallet
w1 = EM.Wallet 1

