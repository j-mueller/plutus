{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE MonoLocalBinds      #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}
-- | A trace is a sequence of actions by simulated wallets that can be run
--   on the mockchain. This module contains the functions needed to build
--   traces.
module Language.Plutus.Contract.Trace(
    ContractTraceState
    , ContractTrace
    , EmulatorAction
    , ctsEvents
    , ctsContract
    , ContractTraceResult(..)
    , ctrEmulatorState
    , ctrTraceState
    , runTrace
    , execTrace
    -- * Constructing 'MonadEmulator' actions
    , runWallet
    , getHooks
    , callEndpoint
    , handleBlockchainEvents
    , addBlocks
    , addEvent
    , addEventAll
    , notifyInterestingAddresses
    , notifySlot
    -- * Running 'MonadEmulator' actions
    , MonadEmulator
    , InitialDistribution
    , withInitialDistribution
    , defaultDist
    -- * Wallets
    , EM.Wallet(..)
    , EM.walletPubKey
    , allWallets
    ) where

import           Control.Lens
import           Control.Monad                                   (void)
import           Control.Monad.Reader
import           Control.Monad.State                             (MonadState, StateT, gets, runStateT)
import           Control.Monad.Trans.Class                       (MonadTrans (..))
import           Data.Bifunctor                                  (Bifunctor (..))
import           Data.Foldable                                   (toList, traverse_)
import           Data.Map                                        (Map)
import qualified Data.Map                                        as Map
import           Data.Row
import           Data.Sequence                                   (Seq)
import qualified Data.Sequence                                   as Seq
import qualified Data.Set                                        as Set

import           Language.Plutus.Contract                        (AddressPrompt, Contract, TxPrompt)
import           Language.Plutus.Contract.Resumable              (ResumableError)
import qualified Language.Plutus.Contract.Resumable              as State
import           Language.Plutus.Contract.Tx                     (UnbalancedTx)
import qualified Language.Plutus.Contract.Wallet                 as Wallet

import qualified Language.Plutus.Contract.Effects.AwaitSlot      as AwaitSlot
import qualified Language.Plutus.Contract.Effects.ExposeEndpoint as Endpoint
import qualified Language.Plutus.Contract.Effects.WatchAddress   as WatchAddress
import qualified Language.Plutus.Contract.Effects.WriteTx        as WriteTx

import           Language.Plutus.Contract.Events                 (Event, Hooks)

import           Ledger.Ada                                      (Ada)
import qualified Ledger.Ada                                      as Ada
import qualified Ledger.AddressMap                               as AM
import           Ledger.Slot                                     (Slot)
import           Ledger.Tx                                       (Address, Tx)

import           Wallet.Emulator                                 (AssertionError, EmulatorAction, EmulatorState,
                                                                  MonadEmulator, Wallet)
import qualified Wallet.Emulator                                 as EM

type InitialDistribution = [(Wallet, Ada)]

type ContractTrace i o m a = StateT (ContractTraceState i o a) m

data ContractTraceState i o a =
    ContractTraceState
        { _ctsEvents   :: Map Wallet (Seq (Event i))
        -- ^ The state of the contract instance (per wallet). To get
        --   the 'Record' of a sequence of events, use
        --   'Language.Plutus.Contract.Resumable.runResumable'.
        , _ctsContract :: Contract i o a
        -- ^ Current state of the contract
        }

makeLenses ''ContractTraceState

initState
    :: [Wallet]
    -> Contract i o a
    -> ContractTraceState i o a
initState wllts = ContractTraceState wallets where
    wallets = Map.fromList $ fmap (,mempty) wllts

-- | Add an event to the wallet's trace
addEvent :: MonadState (ContractTraceState i o a) m => Wallet -> Event i -> m ()
addEvent w e = ctsEvents %= Map.alter go w where
    go = Just . maybe (Seq.singleton e) (|> e)

-- | Get the hooks that a contract is currently waiting for
getHooks
    :: ( Monad m
       , Forall o Monoid
       , Forall o Semigroup
       , AllUniqueLabels o
       )
    => Wallet -> ContractTrace i o m a (Either ResumableError (Hooks o))
getHooks w = do
    contract <- use ctsContract
    evts <- gets (foldMap toList . view (at w) . _ctsEvents)
    return $ State.execResumable evts contract

data ContractTraceResult i o a =
    ContractTraceResult
        { _ctrEmulatorState :: EmulatorState
        -- ^ The emulator state at the end of the test
        , _ctrTraceState    :: ContractTraceState i o a
        -- ^ Final 'ContractTraceState'
        }

makeLenses ''ContractTraceResult

defaultDist :: InitialDistribution
defaultDist = [(EM.Wallet x, 100) | x <- [1..10]]

-- | Add an event to every wallet's trace
addEventAll :: Monad m => Event i -> ContractTrace i o m a ()
addEventAll e = traverse_ (flip addEvent e) allWallets

-- | Run a trace in the emulator and return the
--   final events for each wallet.
execTrace
    :: Contract i o a
    -> ContractTrace i o EmulatorAction a ()
    -> Map Wallet [Event i]
execTrace con action =
    let (e, _) = runTrace con action
    in
        either (const Map.empty) (fmap toList . _ctsEvents . snd) e

-- | Run a trace in the emulator and return the final state alongside the
--   result
runTrace
    :: Contract i o a
    -> ContractTrace i o EmulatorAction a ()
    -> (Either AssertionError ((), ContractTraceState i o a), EmulatorState)
runTrace con action =
    withInitialDistribution defaultDist (runStateT action (initState allWallets con))

-- | Run an 'EmulatorAction' on a blockchain with the given initial distribution
--   of funds to wallets.
withInitialDistribution
    :: [(Wallet, Ada)]
    -> EmulatorAction a
    -> (Either AssertionError a, EmulatorState)
withInitialDistribution dist action =
    let s = EM.emulatorStateInitialDist (Map.fromList (first EM.walletPubKey . second Ada.toValue <$> dist))

        -- make sure the wallets know about the initial transaction
        notifyInitial = void (EM.addBlocksAndNotify (fst <$> dist) 1)
    in EM.runEmulator s (EM.processEmulated notifyInitial >> action)

-- | Run a wallet action in the context of the given wallet, notify the wallets,
--   and return the list of new transactions
runWallet
    :: ( MonadEmulator m )
    => Wallet
    -> EM.MockWallet ()
    -> m [Tx]
runWallet w t = do
    tx <- EM.processEmulated $ EM.runWalletActionAndProcessPending allWallets w t
    _ <- EM.processEmulated $ EM.walletsNotifyBlock allWallets tx
    pure tx

-- | Call the endpoint on the contract
callEndpoint
    :: forall s a b i o m.
       ( MonadEmulator m
       , KnownSymbol s
       , HasType s a i
       , AllUniqueLabels i)
    => Wallet
    -> a
    -> ContractTrace i o m b ()
callEndpoint w = addEvent w . Endpoint.event @s

-- | Balance, sign and submit the unbalanced transaction in the context
--   of the wallet
submitUnbalancedTx
    :: forall a i o m.
      ( MonadEmulator m
      , HasType "tx" () i
      , AllUniqueLabels i
      )
    => Wallet
    -> UnbalancedTx
    -> ContractTrace i o m a [Tx]
submitUnbalancedTx wllt tx =
    addEvent wllt WriteTx.event >> lift (runWallet wllt (Wallet.handleTx tx))

-- | Add the 'LedgerUpdate' event for the given transaction to
--   the traces of all wallets.
addTxEvent
    :: ( MonadEmulator m
       , AddressPrompt i o
       )
    => Tx
    -> ContractTrace i o m a ()
addTxEvent tx = do
    idx <- lift (gets (AM.fromUtxoIndex . view EM.index))
    let event = fmap snd $ Map.toList $ WatchAddress.events idx tx
    traverse_ addEventAll event

-- | Get the unbalanced transactions that the wallet's contract instance
--   would like to submit to the blockchain.
unbalancedTransactions
    :: ( MonadEmulator m
       , HasType "tx" [UnbalancedTx] o
       , Forall o Monoid
       , Forall o Semigroup
       , AllUniqueLabels o
       )
    => Wallet
    -> ContractTrace i o m a [UnbalancedTx]
unbalancedTransactions w = WriteTx.transactions . either (const mempty) id <$> getHooks w

-- | Get the addresses that are of interest to the wallet's contract instance
interestingAddresses
    :: ( MonadEmulator m
       , AddressPrompt i o
       )
    => Wallet
    -> ContractTrace i o m a [Address]
interestingAddresses =
    fmap (Set.toList . WatchAddress.addresses . either (const mempty) id) . getHooks

-- | Add a 'SlotChange' event to the wallet's event trace, informing the
--   contract of the current slot
notifySlot
    :: ( MonadEmulator m
       , HasType "slot" Slot i
       , AllUniqueLabels i
       )
    => Wallet
    -> ContractTrace i o m a ()
notifySlot w = do
    st <- lift $ gets (view (EM.walletStates . at w))
    addEvent w $ AwaitSlot.event (maybe 0 (view EM.walletSlot) st)

-- | Add a number of empty blocks to the blockchain.
addBlocks
    :: ( MonadEmulator m )
    => Integer
    -> ContractTrace i o m a ()
addBlocks i =
    void $ lift $ EM.processEmulated (EM.addBlocksAndNotify allWallets i)

-- | Submit the wallet's pending transactions to the blockchain
--   and inform all wallets about new transactions
handleBlockchainEvents
    :: ( MonadEmulator m
       , AddressPrompt i o
       , TxPrompt i o
       )
    => Wallet
    -> ContractTrace i o m a ()
handleBlockchainEvents wllt = do
    utxs <- unbalancedTransactions wllt
    traverse_ (submitUnbalancedTx wllt >=> traverse_ addTxEvent) utxs

-- | Notify the wallet of all interesting addresses
notifyInterestingAddresses
    :: ( MonadEmulator m
       , AddressPrompt i o
       )
    => Wallet
    -> ContractTrace i o m a ()
notifyInterestingAddresses wllt =
    void $ interestingAddresses wllt >>= lift . runWallet wllt . traverse_ Wallet.startWatching

-- | The wallets used in mockchain simulations by default. There are
--   ten wallets because the emulator comes with ten private keys.
allWallets :: [EM.Wallet]
allWallets = EM.Wallet <$> [1..10]
