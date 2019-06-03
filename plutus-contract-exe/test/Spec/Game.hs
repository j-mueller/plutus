{-# LANGUAGE FlexibleContexts #-}
module Spec.Game where

import           Control.Lens                                  (view)
import           Control.Monad.State                           (gets)
import qualified Data.Aeson                                    as Aeson
import           Data.Foldable                                 (fold, traverse_)
import qualified Data.Map                                      as Map

import           Test.Tasty
import qualified Test.Tasty.HUnit                              as HUnit

import qualified Language.Plutus.Contract.Event                as Event
import qualified Language.Plutus.Contract.Wallet               as Wallet
import qualified Ledger.Ada                                    as Ada
import qualified Ledger.AddressMap                             as AM
import qualified Wallet.Emulator                               as EM

import           Examples.Game                                 (GuessParams (..), LockParams (..), game)
import           Language.Plutus.Contract.Contract             as Con
import           Language.Plutus.Contract.Event                (Step (..))
import           Language.PlutusTx.Coordination.Contracts.Game (gameAddress)

import           Spec.HUnit                                    (assertEmulatorAction, assertEndpoint,
                                                                assertInterestingAddress, assertTx, run)

tests :: TestTree
tests = testGroup "game"
    [ HUnit.testCaseSteps "'lock' endpoint is available" $
        \lg -> assertEmulatorAction [(w1, 10)] initialTrace (assertEndpoint lg "lock")
    , HUnit.testCaseSteps "game address" $
        \lg -> assertEmulatorAction [(w1, 10)] initialTrace (assertInterestingAddress lg gameAddress)
    , HUnit.testCaseSteps "submit locking transaction" $
        \lg -> assertEmulatorAction [(w1, 10)] submitTrace (assertTx lg (const True))
    , HUnit.testCaseSteps "'guess' endpoint is available" $
        \lg -> assertEmulatorAction [(w1, 100)] lockTrace (assertEndpoint lg "guess")
    , HUnit.testCase "unlock funds" $
        assertEmulatorAction [(w1, 100)] unlockTrace (\_ -> pure ())
    ]

-- | A mockchain trace that submits the 'lock' transaction to the blockchain
submitTrace :: EM.MonadEmulator m => m Step
submitTrace =
    let
        inp = Event.endpoint "lock" (Aeson.toJSON $ LockParams "secret" 10)
    in
        pure (fst $ Con.applyInput inp game)

-- | A mockchain trace that submits the 'lock' transaction to the blockchain
lockTrace :: EM.MonadEmulator m => m Step
lockTrace = do
    let
        run' = run [w1, w2]

        inp = Event.endpoint "lock" (Aeson.toJSON $ LockParams "secret" 10)
        (step', rest') = Con.applyInput inp game

    block <- run' w1 (traverse_ Wallet.handleTx (Event.stepTransactions step'))
    idx <- gets (AM.fromUtxoIndex . view EM.index)

    let events = foldMap (fmap snd . Map.toList . Event.txEvents idx) block
        (step'', _) = Con.applyInputs rest' events

    pure (fold step'')

-- | A mockchain trace that submits the 'lock' transaction to the blockchain
unlockTrace :: EM.MonadEmulator m => m ()
unlockTrace = do
    let
        run' = run [w1, w2]

        inp = Event.endpoint "lock" (Aeson.toJSON $ LockParams "secret" 10)
        (step1, rest1) = Con.applyInput inp game

    block <- run' w1 (traverse_ Wallet.handleTx (Event.stepTransactions step1))
    idx <- gets (AM.fromUtxoIndex . view EM.index)

    let events = foldMap (fmap snd . Map.toList . Event.txEvents idx) block
        (_, rest2) = Con.applyInputs rest1 events
        input2 = Event.endpoint "guess" (Aeson.toJSON $ GuessParams "secret")
        (step3, _) = Con.applyInput input2 rest2

    _ <- run' w2 (traverse_ Wallet.handleTx (Event.stepTransactions step3))
    EM.ownFundsEqual w2 (Ada.adaValueOf 10)

-- | A mockchain trace that submits the 'lock' transaction to the blockchain
initialTrace :: EM.MonadEmulator m => m Step
initialTrace = pure (fst $ Con.drain game)

w1, w2 :: EM.Wallet
w1 = EM.Wallet 1
w2 = EM.Wallet 2
