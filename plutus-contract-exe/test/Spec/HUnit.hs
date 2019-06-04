{-# LANGUAGE FlexibleContexts #-}
-- | Testing contracts with HUnit
module Spec.HUnit(
    -- * Emulator integration
      withInitialDistribution
    , run
    , handleInputs
    , callEndpoint
    -- * Making assertions about 'Step' values
    , assertEndpoint
    , assertInterestingAddress
    , assertEmulatorAction
    , assertTx

    ) where

import           Control.Lens                         (view)
import           Control.Monad                        (void)
import           Control.Monad.State                  (gets)
import qualified Data.Aeson                           as Aeson
import           Data.Bifunctor                       (Bifunctor (..))
import           Data.Foldable                        (fold, traverse_)
import qualified Data.Map                             as Map
import qualified Data.Set                             as Set
import           Language.Plutus.Contract             (PlutusContract)
import           Language.Plutus.Contract.Contract    as Con
import           Language.Plutus.Contract.Event       (Event)
import qualified Language.Plutus.Contract.Event       as Event
import           Language.Plutus.Contract.Step        (Step (..))
import qualified Language.Plutus.Contract.Step        as Step
import           Language.Plutus.Contract.Transaction (UnbalancedTx)
import qualified Language.Plutus.Contract.Wallet      as Wallet

import           Ledger.Ada                           (Ada)
import qualified Ledger.Ada                           as Ada
import qualified Ledger.AddressMap                    as AM
import           Ledger.Tx                            (Address, Tx)
import qualified Test.Tasty.HUnit                     as HUnit
import           Wallet.Emulator                      (AssertionError, EmulatorAction, EmulatorState, MonadEmulator,
                                                       Wallet)
import qualified Wallet.Emulator                      as EM

-- | Run an 'EmulatorAction' on a blockchain with the given initial distribution
--   of funds to wallets.
withInitialDistribution :: [(Wallet, Ada)] -> EmulatorAction a -> (Either AssertionError a, EmulatorState)
withInitialDistribution dist action =
    let s = EM.emulatorStateInitialDist (Map.fromList (first EM.walletPubKey . second Ada.toValue <$> dist))

        -- make sure the wallets know about the initial transaction
        notifyInitial = void (EM.addBlocksAndNotify (fst <$> dist) 1)
    in EM.runEmulator s (EM.processEmulated notifyInitial >> action)

-- | Run a wallet action in the context of the given wallet, notify the wallets,
--   and return the list of new transactions
run :: MonadEmulator m => [Wallet] -> Wallet -> EM.MockWallet () -> m [Tx]
run ws w = EM.processEmulated . EM.runWalletActionAndProcessPending ws w

assertEndpoint :: (String -> IO ()) -> String -> Step -> HUnit.Assertion
assertEndpoint lg nm stp =
    if nm `Set.member` stepEndpoints stp
    then pure ()
    else do
        lg (show stp)
        HUnit.assertFailure ("endpoint " ++ nm ++ " is not available")

assertInterestingAddress :: (String -> IO ()) -> Address -> Step -> HUnit.Assertion
assertInterestingAddress lg addr stp =
    if addr `Set.member` stepAddresses stp
    then pure ()
    else do
        lg (show stp)
        HUnit.assertFailure ("address" ++ show addr ++ " not found")

assertEmulatorAction :: [(Wallet, Ada)] -> EmulatorAction a -> (a -> HUnit.Assertion) -> HUnit.Assertion
assertEmulatorAction dist trace assertion = do
    let (r, _) = withInitialDistribution dist trace
    either (HUnit.assertFailure . show) assertion r

assertTx :: (String -> IO ()) -> (UnbalancedTx -> Bool) -> Step -> HUnit.Assertion
assertTx lg flt stp =
    if any flt (stepTransactions stp)
    then pure ()
    else do
        lg (show stp)
        HUnit.assertFailure "'assertTx' failed"

-- | Call the endpoint on the contract, submit all transactions
--   to the mockchain in the context of the given wallet, and 
--   return the new contract together with the list of steps.
callEndpoint
    :: ( MonadEmulator m
       , Aeson.ToJSON b )
    => Wallet
    -> String
    -> b
    -> PlutusContract a
    -> m ([Step], PlutusContract a)
callEndpoint w nm vl =
    handleInputs w [Event.endpoint nm (Aeson.toJSON vl)]

-- | Apply the contract to the list of events, submit
--   all transactions that come out to the mockchain
--   in the context of the given wallet, and return
--   the new contract together with the list of steps
handleInputs
    :: ( MonadEmulator m )
    => Wallet
    -> [Event]
    -> PlutusContract a
    -> m ([Step], PlutusContract a)
handleInputs wllt ins contract = do
    let (step1, rest1) = Con.applyInputs contract ins
        run' = run (EM.Wallet <$> [1..10])
    block <- run' wllt (traverse_ Wallet.handleTx (Step.stepTransactions $ fold step1))
    idx <- gets (AM.fromUtxoIndex . view EM.index)
    let events = foldMap (fmap snd . Map.toList . Event.txEvents idx) block
    pure (Con.applyInputs rest1 events)
