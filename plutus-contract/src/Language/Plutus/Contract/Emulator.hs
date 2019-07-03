{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}
-- | Some conveniences for running Plutus contracts
--   in the emulator.
module Language.Plutus.Contract.Emulator(
    ContractTraceState
    , ContractTrace
    , ctsEvents
    , ctsContract
    , ContractTraceResult(..)
    , ctrEmulatorState
    , ctrTraceState
    , runTrace
    , defaultDist
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

import           Control.Lens                         (makeLenses, use, view, (<>=))
import           Control.Monad                        (void)
import           Control.Monad.State                  (StateT, gets, runStateT)
import           Control.Monad.Trans.Class            (MonadTrans (..))
import           Control.Monad.Writer
import qualified Data.Aeson                           as Aeson
import           Data.Bifunctor                       (Bifunctor (..))
import           Data.Foldable                        (toList, traverse_)
import qualified Data.Map                             as Map
import           Data.Sequence                        (Seq)
import qualified Data.Sequence                        as Seq

import           Language.Plutus.Contract.Contract    as Con
import           Language.Plutus.Contract.Event       (Event)
import qualified Language.Plutus.Contract.Event       as Event
import           Language.Plutus.Contract.Hooks       (Hooks (..))
import qualified Language.Plutus.Contract.Hooks       as Hooks
import qualified Language.Plutus.Contract.Wallet      as Wallet

import           Ledger.Ada                           (Ada)
import qualified Ledger.Ada                           as Ada
import qualified Ledger.AddressMap                    as AM
import           Ledger.Tx                            (Tx)
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

data ContractTraceResult a =
    ContractTraceResult
        { _ctrEmulatorState :: EmulatorState
        -- ^ The emulator state at the end of the test
        , _ctrTraceState    :: ContractTraceState a
        -- ^ Final 'ContractTraceState'
        }

makeLenses ''ContractTraceResult

type ContractTrace m a b = StateT (ContractTraceState a) m b

runTrace :: ContractPrompt Maybe a -> ContractTrace EmulatorAction a () -> (Either AssertionError ((), ContractTraceState a), EmulatorState)
runTrace con action = withInitialDistribution defaultDist (runStateT action (mkState con))

mkState :: ContractPrompt Maybe a -> ContractTraceState a
mkState = ContractTraceState mempty

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
