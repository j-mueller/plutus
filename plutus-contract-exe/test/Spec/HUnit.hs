{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}
-- | Testing contracts with HUnit
module Spec.HUnit(
    -- * Making assertions about 'Hooks' values
      ContractTraceState
    , ctsEvents
    , ctsContract
    , ContractTestResult
    , ctrEmulatorState
    , ctrTraceState
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
    -- * Constructing 'MonadEmulator' actions
    , initContract
    , runWallet
    , event_
    , getHooks
    , handleInputs
    , callEndpoint
    -- * Running 'MonadEmulator' actions
    , InitialDistribution
    , withInitialDistribution
    ) where

import           Control.Lens                         (at, makeLenses, to, use, view, (<>=), (^.))
import           Control.Monad                        (void)
import           Control.Monad.State                  (StateT, gets, execStateT, runStateT)
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

type InitialDistribution = [(Wallet, Ada)]

data ContractTraceState a =
    ContractTraceState
        { _ctsEvents   :: Seq Event
        -- ^ Events that were fed to the contract
        , _ctsContract :: ContractPrompt Maybe a
        -- ^ Current state of the contract
        }

makeLenses ''ContractTraceState

data ContractTestResult a =
    ContractTestResult
        { _ctrEmulatorState :: EmulatorState
        -- ^ The emulator state at the end of the test
        , _ctrTraceState    :: ContractTraceState a
        -- ^ Final 'ContractTraceState'
        }

makeLenses ''ContractTestResult

type TracePredicate a = InitialDistribution -> Predicate (ContractTestResult a)

type ContractTrace m a b = StateT (ContractTraceState a) m b

mkState :: ContractPrompt Maybe a -> ContractTraceState a
mkState = ContractTraceState mempty

hooks :: ContractTestResult a -> Hooks
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
        case withInitialDistribution defaultDist (runStateT action (mkState con)) of
            (Left err, _) -> 
                HUnit.assertFailure $ "EmulatorAction failed. " ++ show err
            (Right (_, st), ms) ->
                let dt = ContractTestResult ms st in
                HUnit.assertBool nm (getPredicate (predicate defaultDist) dt)

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
runWallet :: MonadEmulator m => [Wallet] -> Wallet -> EM.MockWallet () -> m [Tx]
runWallet ws w = EM.processEmulated . EM.runWalletActionAndProcessPending ws w

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
    \ContractTestResult{_ctrEmulatorState = st} ->
        let initialValue = foldMap Ada.toValue (Map.fromList initialDist ^. at w)
            finalValue   = fromMaybe mempty (EM.fundsDistribution st ^. at w)
        in initialValue `V.plus` dlt == finalValue

-- | Initial distribution of 100 Ada to each wallet.
defaultDist :: [(Wallet, Ada)]
defaultDist = [(EM.Wallet x, 100) | x <- [1..10]]

initContract :: ContractPrompt Maybe a -> Hooks
initContract con = execWriter (runStateT (runContract con) [])

event_ :: Monad m => Event -> ContractTrace m a ()
event_ e = ctsEvents <>= Seq.singleton e

getHooks :: Monad m => ContractTrace m a Hooks
getHooks = do
    contract <- use ctsContract
    evts <- gets (toList .  _ctsEvents)
    return $ execWriter (runStateT (runContract contract) evts)

-- | Call the endpoint on the contract, submit all transactions
--   to the mockchain in the context of the given wallet, and
--   return the new contract together with the list of steps.
callEndpoint
    :: ( MonadEmulator m
       , Aeson.ToJSON b )
    => Wallet
    -> String
    -> b
    -> ContractTrace m a ()
callEndpoint w nm vl = handleInputs w [Event.endpoint nm (Aeson.toJSON vl)]

-- | Apply the contract to the list of events, submit
--   all transactions that come out to the mockchain
--   in the context of the given wallet, and return
--   the new contract together with the list of steps
handleInputs
    :: ( MonadEmulator m )
    => Wallet
    -> [Event]
    -> ContractTrace m a ()
handleInputs wllt ins = do
    _ <- traverse_ event_ ins
    step1 <- getHooks
    let run' = runWallet (EM.Wallet <$> [1..10])
        txns = Hooks.transactions step1

    block <- lift (run' wllt (traverse_ Wallet.handleTx txns))
    traverse_ event_ (fmap (const Event.txSubmission) txns)
    idx <- lift (gets (AM.fromUtxoIndex . view EM.index))

    let events = foldMap (fmap snd . Map.toList . Event.txEvents idx) block
    traverse_ event_ events
