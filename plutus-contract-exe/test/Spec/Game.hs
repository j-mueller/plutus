{-# LANGUAGE FlexibleContexts #-}
module Spec.Game where

import           Control.Lens                                  (use)
import qualified Data.Aeson                                    as Aeson
import           Data.Foldable                                 (fold, traverse_)
import qualified Data.Map                                      as Map

import           Test.Tasty
import qualified Test.Tasty.HUnit                              as HUnit

import qualified Language.Plutus.Contract.Event                as Event
import qualified Language.Plutus.Contract.Wallet               as Wallet
import qualified Ledger.AddressMap                             as AM
import qualified Wallet.Emulator                               as EM

import           Examples.Game                                 (GuessParams (..), LockParams (..), game)
import           Language.Plutus.Contract.Contract             as Con
import           Language.Plutus.Contract.Event                (Step (..))
import           Language.PlutusTx.Coordination.Contracts.Game (gameAddress)

import           Spec.HUnit                                    (assertEmulatorAction, assertEndpoint,
                                                                assertInterestingAddress, assertTx, run,
                                                                withInitialDistribution)

tests :: TestTree
tests = testGroup "game"
    [ HUnit.testCaseSteps "'lock' endpoint is available" $ \lg -> assertEmulatorAction [(EM.Wallet 1, 10)] initialTrace (assertEndpoint lg "lock")
    , HUnit.testCaseSteps "game address" $ \lg -> assertEmulatorAction [(EM.Wallet 1, 10)] initialTrace (assertInterestingAddress lg gameAddress)
    , HUnit.testCaseSteps "submit locking transaction" submitLockingTxn
    , HUnit.testCaseSteps "lock and unlock" gameOne
    ]

gameOne :: (String -> IO ()) -> HUnit.Assertion
gameOne lg = do
    let (r, _) = withInitialDistribution [(EM.Wallet 1, 10)] gameTrace
    case r of
        Left _     -> HUnit.assertFailure "Left"
        Right step -> assertTx lg (const True) step

submitLockingTxn :: (String -> IO ()) -> HUnit.Assertion
submitLockingTxn lg = do
    let (r, _) = withInitialDistribution [(EM.Wallet 1, 10)] submitTrace
    case r of
        Left _     -> HUnit.assertFailure "Left"
        Right step -> assertTx lg (const True) step

-- | A mockchain trace that submits the 'lock' transaction to the blockchain
submitTrace :: EM.MonadEmulator m => m Step
submitTrace = do
    let
        run' = run [w1, w2]

        inp = Event.endpoint "lock" (Aeson.toJSON $ LockParams "secret" 10)
        (step', rest') = Con.applyInput inp game
    block <- run' w1 (traverse_ Wallet.handleTx (Event.stepTransactions step'))
    idx <- AM.fromUtxoIndex <$> use EM.index

    let events = foldMap (fmap snd . Map.toList . Event.txEvents idx) block
        (step'', _) = Con.applyInputs rest' events

    pure (fold step'')

-- | A mockchain trace that submits the 'lock' transaction to the blockchain
initialTrace :: EM.MonadEmulator m => m Step
initialTrace = pure (fst $ Con.drain game)

-- | A mockchain trace that plays a successful round of the guessing game.
gameTrace :: EM.MonadEmulator m => m Step
gameTrace = do
    let
        run' = run [w1, w2]

        inp = Event.endpoint "lock" (Aeson.toJSON $ LockParams "secret" 10)
        (step', rest') = Con.applyInput inp (snd (Con.drain game))

    block <- run' w1 (traverse_ Wallet.handleTx (Event.stepTransactions step'))
    idx <- AM.fromUtxoIndex <$> use EM.index

    let
        events = foldMap (fmap snd . Map.toList . Event.txEvents idx) block
        inp' = Event.endpoint "guess" (Aeson.toJSON $ GuessParams "secret")
        (_, rest'') = Con.applyInputs rest' events
        (step'', _) = Con.applyInput inp' rest''

    pure step''

w1, w2 :: EM.Wallet
w1 = EM.Wallet 1
w2 = EM.Wallet 2
