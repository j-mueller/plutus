{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}
-- | Testing contracts with HUnit
module Spec.HUnit(
      module X
    , TracePredicate
    , Spec.HUnit.not
    , endpointAvailable
    , interestingAddress
    , assertResult
    , tx
    , anyTx
    , walletFundsChange
    , waitingForSlot
    -- * Checking predicates
    , checkPredicate
    ) where

import           Control.Lens                         (at, makeLenses, to, use, view, (<>=), (^.))
import           Control.Monad                        (void)
import           Control.Monad.State                  (StateT, execStateT, gets, runStateT)
import           Control.Monad.Trans.Class            (MonadTrans (..))
import           Control.Monad.Writer
import qualified Data.Aeson                           as Aeson
import           Data.Bifunctor                       (Bifunctor (..))
import           Data.Foldable                        (toList, traverse_)
import           Data.Functor.Contravariant           (Predicate (..))
import qualified Data.Map                             as Map
import           Data.Maybe                           (fromMaybe)
import           Data.Sequence                        (Seq)
import qualified Data.Sequence                        as Seq
import qualified Data.Set                             as Set
import qualified Test.Tasty.HUnit                     as HUnit
import           Test.Tasty.Providers                 (TestTree)

import           Language.Plutus.Contract.Contract    as Con
import           Language.Plutus.Contract.Event       (Event)
import qualified Language.Plutus.Contract.Event       as Event
import           Language.Plutus.Contract.Hooks       (Hooks (..))
import qualified Language.Plutus.Contract.Hooks       as Hooks
import           Language.Plutus.Contract.Transaction (UnbalancedTx)
import qualified Language.Plutus.Contract.Wallet      as Wallet

import           Ledger.Ada                           (Ada)
import qualified Ledger.Ada                           as Ada
import qualified Ledger.AddressMap                    as AM
import           Ledger.Slot                          (Slot)
import           Ledger.Tx                            (Address, Tx)
import           Ledger.Value                         (Value)
import qualified Ledger.Value                         as V
import           Wallet.Emulator                      (AssertionError, EmulatorAction, EmulatorState, MonadEmulator,
                                                       Wallet)
import qualified Wallet.Emulator                      as EM

import Language.Plutus.Contract.Emulator as X

type TracePredicate a = InitialDistribution -> Predicate (ContractTraceResult a)

hooks :: ContractTraceResult a -> Hooks
hooks rs =
    let evts = rs ^. ctrTraceState . ctsEvents . to toList
        con  = rs ^. ctrTraceState . ctsContract
    in execWriter (execStateT (runContract con) evts)

not :: TracePredicate a -> TracePredicate a
not p a = Predicate $ \b -> Prelude.not (getPredicate (p a) b)

checkPredicate
    :: String
    -> ContractPrompt Maybe a
    -> TracePredicate a
    -> ContractTrace EmulatorAction a ()
    -> TestTree
checkPredicate nm con predicate action =
    HUnit.testCase nm $
        case runTrace con action of
            (Left err, _) ->
                HUnit.assertFailure $ "EmulatorAction failed. " ++ show err
            (Right (_, st), ms) ->
                let dt = ContractTraceResult ms st in
                HUnit.assertBool nm (getPredicate (predicate defaultDist) dt)

endpointAvailable :: String -> TracePredicate a
endpointAvailable nm _ = Predicate $ \r ->
    nm `Set.member` Hooks.activeEndpoints (hooks r)

interestingAddress :: Address -> TracePredicate a
interestingAddress addr _ = Predicate $ \r ->
        addr `Set.member` Hooks.addresses (hooks r)

tx :: (UnbalancedTx -> Bool) -> TracePredicate a
tx flt _ = Predicate $ \r ->
    any flt (Hooks.transactions (hooks r))

waitingForSlot :: Slot -> TracePredicate a
waitingForSlot sl _ = Predicate $ \r ->
    Just sl == Hooks.nextSlot (hooks r)

anyTx :: TracePredicate a
anyTx = tx (const True)

assertResult :: (Maybe a -> Bool) -> TracePredicate a
assertResult p _ = Predicate $ \rs ->
    let evts = rs ^. ctrTraceState . ctsEvents . to toList
        con  = rs ^. ctrTraceState . ctsContract
        result = fst (runWriter (runStateT (runContract con) evts))
    in p (fst result)

walletFundsChange :: Wallet -> Value -> TracePredicate a
walletFundsChange w dlt initialDist = Predicate $
    \ContractTraceResult{_ctrEmulatorState = st} ->
        let initialValue = foldMap Ada.toValue (Map.fromList initialDist ^. at w)
            finalValue   = fromMaybe mempty (EM.fundsDistribution st ^. at w)
        in initialValue `V.plus` dlt == finalValue
