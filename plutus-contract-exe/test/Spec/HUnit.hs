{-# LANGUAGE FlexibleContexts #-}
-- | Testing contracts with HUnit
module Spec.HUnit(
      withInitialDistribution
    , run
    , assertEndpoint
    , assertInterestingAddress
    , assertEmulatorAction
    , assertTx
    ) where

import           Data.Bifunctor                 (Bifunctor (..))
import qualified Data.Map                       as Map
import qualified Data.Set                       as Set
import           Language.Plutus.Contract.Event (Step (..))
import Language.Plutus.Contract.Transaction     (UnbalancedTx)
import           Ledger.Ada                     (Ada)
import qualified Ledger.Ada                     as Ada
import           Ledger.Tx                      (Address, Tx)
import qualified Test.Tasty.HUnit               as HUnit
import           Wallet.Emulator                (AssertionError, EmulatorAction, EmulatorState, MonadEmulator, Wallet)
import qualified Wallet.Emulator                as EM

-- | Run an 'EmulatorAction' on a blockchain with the given initial distribution
--   of funds to wallets.
withInitialDistribution :: [(Wallet, Ada)] -> EmulatorAction a -> (Either AssertionError a, EmulatorState)
withInitialDistribution dist =
    let s = EM.emulatorStateInitialDist (Map.fromList (first EM.walletPubKey . second Ada.toValue <$> dist))
    in EM.runEmulator s

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