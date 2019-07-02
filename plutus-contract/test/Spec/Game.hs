{-# LANGUAGE FlexibleContexts #-}
module Spec.Game where

import qualified Data.Aeson                                    as Aeson
import           Test.Tasty

import qualified Language.Plutus.Contract.Event                as Event
import qualified Ledger.Ada                                    as Ada
import qualified Wallet.Emulator                               as EM

import           Examples.Game                                 (GuessParams (..), LockParams (..), game)
import           Language.PlutusTx.Coordination.Contracts.Game (gameAddress)

import           Spec.HUnit

tests :: TestTree
tests = testGroup "game"
    [ checkPredicate "Expose 'lock' endpoint and watch game address"
        game
        (endpointAvailable "lock" <> interestingAddress gameAddress)
        $ pure ()

    , checkPredicate "'lock' endpoint submits a transaction"
        game
        anyTx
        $ event_ (Event.endpoint "lock" (Aeson.toJSON $ LockParams "secret" 10))

    , checkPredicate "'guess' endpoint is available after locking funds"
        game
        (endpointAvailable "guess")
        $ callEndpoint w1 "lock" (LockParams "secret" 10)

    , checkPredicate "unlock funds"
        game
        (walletFundsChange w2 (Ada.adaValueOf 10) <> walletFundsChange w1 (Ada.adaValueOf (-10)))
        $ callEndpoint w1 "lock" (LockParams "secret" 10)
            >> callEndpoint w2 "guess" (GuessParams "secret")
    ]

w1, w2 :: EM.Wallet
w1 = EM.Wallet 1
w2 = EM.Wallet 2
