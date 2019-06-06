{-# LANGUAGE TypeApplications #-}
module Spec.Contract(tests) where

import           Test.Tasty

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
      (waitingForSlot 10)
      (fst <$> initContract (slotGeq 10))

  , checkPredicate "selectEither"
      (waitingForSlot 5)
      (fst <$> initContract (selectEither (slotGeq 10) (slotGeq 5)))

  , checkPredicate "until"
      (waitingForSlot 5)
      (fst <$> initContract (slotGeq 10 `Con.until` 5))

  , checkPredicate "both"
      (waitingForSlot 10)
      (fst <$> initContract (Con.both (slotGeq 10) (slotGeq 20)))

  , checkPredicate "fundsAtAddressGt"
      (interestingAddress gameAddress)
      (fst <$> initContract (fundsAtAddressGt gameAddress (Ada.adaValueOf 10)))

  , checkPredicate "watchAddressUntil"
      (interestingAddress gameAddress <> waitingForSlot 5)
      (fst <$> initContract (watchAddressUntil gameAddress 5))

  , checkPredicate "endpoint"
      (endpointAvailable "ep")
      (fst <$> initContract (endpoint @() "ep"))

  , checkPredicate "call endpoint (1)"
      (endpointAvailable "1")
      (fst <$> initContract (endpoint @Int "1" >> endpoint @Int "2"))

  , checkPredicate "call endpoint (2)"
      (endpointAvailable "2" <> not (endpointAvailable "1"))
      (let con =
            Con.offer
                (Event.endpoint "1" (Aeson.toJSON (1 :: Int)))
                (snd $ Con.drain $ endpoint @Int "1" >> endpoint @Int "2")
      in fst <$> initContract con)

  , checkPredicate "call endpoint (3)"
      (endpointAvailable "2" <> not (endpointAvailable "1"))
      (fst <$> callEndpoint w1 "1" (1::Int) (endpoint @Int "1" >> endpoint @Int "2"))
  ]

w1, w2 :: EM.Wallet
w1 = EM.Wallet 1
w2 = EM.Wallet 2

