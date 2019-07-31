{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
module Spec.Game(tests) where

import           Test.Tasty
import qualified Test.Tasty.HUnit                              as HUnit
import qualified Spec.Lib                                      as Lib
import           Language.Plutus.Contract.Test
import qualified Language.Plutus.Contract.Prompt.Event as Event
import qualified Language.PlutusTx                             as PlutusTx
import           Language.PlutusTx.Coordination.Contracts.Game (GuessParams (..), LockParams (..), game, gameAddress,
                                                                gameValidator, validateGuess)
import qualified Ledger.Ada                                    as Ada

w1, w2 :: Wallet
w1 = Wallet 1
w2 = Wallet 2

tests :: TestTree
tests = testGroup "game"
    [ checkPredicate "Expose 'lock' endpoint and watch game address"
        game
        (endpointAvailable w1 "lock" <> interestingAddress w1 gameAddress)
        $ pure ()

    , checkPredicate "'lock' endpoint submits a transaction"
        game
        (anyTx w1)
        $ addEvent w1 (Event.endpointJson "lock" (LockParams "secret" 10))

    , checkPredicate "'guess' endpoint is available after locking funds"
        game
        (endpointAvailable w2 "guess")
        $ callEndpoint w1 "lock" (LockParams "secret" 10)
            >> notifyInterestingAddresses w2
            >> handleBlockchainEvents w1

    , checkPredicate "unlock funds"
        game
        (walletFundsChange w2 (Ada.adaValueOf 10)
            <> walletFundsChange w1 (Ada.adaValueOf (-10)))
        $ callEndpoint w1 "lock" (LockParams "secret" 10)
            >> notifyInterestingAddresses w2
            >> handleBlockchainEvents w1
            >> callEndpoint w2 "guess" (GuessParams "secret")
            >> handleBlockchainEvents w2
    , Lib.goldenPir "test/Spec/game.pir" $$(PlutusTx.compile [|| validateGuess ||])
    , HUnit.testCase "script size is reasonable" (Lib.reasonable gameValidator 25000)
    ]
