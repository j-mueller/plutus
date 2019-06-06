{-# LANGUAGE FlexibleContexts #-}
module Spec.Game where

import qualified Data.Aeson                                    as Aeson
import           Test.Tasty

import qualified Language.Plutus.Contract.Event                as Event
import qualified Ledger.Ada                                    as Ada
import qualified Wallet.Emulator                               as EM

import           Examples.Game                                 (GuessParams (..), LockParams (..), game)
import           Language.Plutus.Contract.Contract             as Con
import           Language.PlutusTx.Coordination.Contracts.Game (gameAddress)

import           Spec.HUnit

tests :: TestTree
tests = testGroup "game"
    [ checkPredicate "Expose 'lock' endpoint and watch game address"
        (endpointAvailable "lock" <> interestingAddress gameAddress)
        (fst <$> initContract game)

    , checkPredicate "'lock' endpoint submits a transaction" anyTx $
        let e = Event.endpoint "lock" (Aeson.toJSON $ LockParams "secret" 10)
        in pure (fst $ Con.drain $ Con.applyInput e game)

    , checkPredicate "'guess' endpoint is available after locking funds" 
        (endpointAvailable "guess")
        $ fst <$> callEndpoint w1 "lock" (LockParams "secret" 10) game

    , checkPredicate "unlock funds" 
        (walletFundsChange w2 (Ada.adaValueOf 10) <> walletFundsChange w1 (Ada.adaValueOf (-10)))
        $ callEndpoint w1 "lock" (LockParams "secret" 10) game
            >>= callEndpoint w2 "guess" (GuessParams "secret") . snd
    ]

w1, w2 :: EM.Wallet
w1 = EM.Wallet 1
w2 = EM.Wallet 2
