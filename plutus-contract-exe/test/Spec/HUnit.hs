{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
-- | Testing contracts with HUnit
module Spec.HUnit(
    -- * Making assertions about 'Step' values
      TracePredicate
    , MockchainResult
    , endpointAvailable
    , interestingAddress
    , tx
    , anyTx
    , walletFundsChange
    -- * Checking predicates
    , checkPredicate
    -- * Constructing 'MonadEmulator' actions
    , runWallet
    , handleInputs
    , callEndpoint
    -- * Running 'MonadEmulator' actions
    , InitialDistribution
    , withInitialDistribution
    , assertEmulatorAction
    , assertEmulatorAction'
    ) where

import           Control.Lens                         (at, view, (^.))
import           Control.Monad                        (void)
import           Control.Monad.State                  (gets)
import qualified Data.Aeson                           as Aeson
import           Data.Bifunctor                       (Bifunctor (..))
import           Data.Foldable                        (fold, traverse_)
import           Data.Functor.Contravariant           (Predicate (..))
import qualified Data.Map                             as Map
import           Data.Maybe                           (fromMaybe)
import qualified Data.Set                             as Set
import qualified Test.Tasty.HUnit                     as HUnit
import           Test.Tasty.Providers                 (TestTree)

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
import           Ledger.Value                         (Value)
import qualified Ledger.Value                         as V
import           Wallet.Emulator                      (AssertionError, EmulatorAction, EmulatorState, MonadEmulator,
                                                       Wallet)
import qualified Wallet.Emulator                      as EM

type InitialDistribution = [(Wallet, Ada)]

type MockchainResult a = (Either AssertionError a, EmulatorState)

type TracePredicate a = InitialDistribution -> Predicate (MockchainResult a)

checkPredicate :: String -> TracePredicate a -> EmulatorAction a -> TestTree
checkPredicate nm predicate action =
    HUnit.testCase nm $
        HUnit.assertBool nm (getPredicate (predicate defaultDist) (withInitialDistribution defaultDist action))

-- | Run an 'EmulatorAction' on a blockchain with the given initial distribution
--   of funds to wallets.
withInitialDistribution :: [(Wallet, Ada)] -> EmulatorAction a -> MockchainResult a
withInitialDistribution dist action =
    let s = EM.emulatorStateInitialDist (Map.fromList (first EM.walletPubKey . second Ada.toValue <$> dist))

        -- make sure the wallets know about the initial transaction
        notifyInitial = void (EM.addBlocksAndNotify (fst <$> dist) 1)
    in EM.runEmulator s (EM.processEmulated notifyInitial >> action)

-- | Run a wallet action in the context of the given wallet, notify the wallets,
--   and return the list of new transactions
runWallet :: MonadEmulator m => [Wallet] -> Wallet -> EM.MockWallet () -> m [Tx]
runWallet ws w = EM.processEmulated . EM.runWalletActionAndProcessPending ws w

endpointAvailable :: String -> TracePredicate Step
endpointAvailable nm _ = Predicate $ \case
    (Left _, _) -> False
    (Right stp, _) -> nm `Set.member` stepEndpoints stp

interestingAddress :: Address -> TracePredicate Step
interestingAddress addr _ = Predicate $ \case
    (Left _, _) -> False
    (Right stp, _) -> addr `Set.member` stepAddresses stp

tx :: (UnbalancedTx -> Bool) -> TracePredicate Step
tx flt _ = Predicate $ \case
    (Left _, _) -> False
    (Right stp, _) -> any flt (stepTransactions stp)

anyTx :: TracePredicate Step
anyTx = tx (const True)

walletFundsChange :: Wallet -> Value -> TracePredicate a
walletFundsChange w dlt initialDist = Predicate $ \(_, st) ->
    let initialValue = foldMap Ada.toValue (Map.fromList initialDist ^. at w)
        finalValue   = fromMaybe mempty (EM.fundsDistribution st ^. at w)
    in initialValue `V.plus` dlt == finalValue

assertEmulatorAction :: [(Wallet, Ada)] -> EmulatorAction a -> (a -> HUnit.Assertion) -> HUnit.Assertion
assertEmulatorAction dist trace assertion = do
    let (r, _) = withInitialDistribution dist trace
    either (HUnit.assertFailure . show) assertion r

assertEmulatorAction' :: EmulatorAction a -> (a -> HUnit.Assertion) -> HUnit.Assertion
assertEmulatorAction' = assertEmulatorAction defaultDist

-- | Initial distribution of 100 Ada to each wallet.
defaultDist :: [(Wallet, Ada)]
defaultDist = [(EM.Wallet x, 100) | x <- [1..10]]

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
        run' = runWallet (EM.Wallet <$> [1..10])
    block <- run' wllt (traverse_ Wallet.handleTx (Step.stepTransactions $ fold step1))
    idx <- gets (AM.fromUtxoIndex . view EM.index)
    let events = foldMap (fmap snd . Map.toList . Event.txEvents idx) block
    pure (Con.applyInputs rest1 events)
