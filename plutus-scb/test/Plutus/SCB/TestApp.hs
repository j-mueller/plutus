{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}

-- | A test version of the 'App' stack which runs all operations in memory.
-- No networking, no filesystem.
module Plutus.SCB.TestApp
    ( runScenario
    , sync
    , TestAppEffects
    , getFollowerID
    , valueAt
    ) where

import           Cardano.Node.Follower                         (NodeFollowerEffect)
import qualified Cardano.Node.Follower                         as NodeFollower
import           Cardano.Node.Mock                             (NodeServerEffects)
import qualified Cardano.Node.Mock                             as NodeServer
import           Cardano.Node.RandomTx                         (GenRandomTx)
import           Cardano.Node.Types                            (AppState, FollowerID, NodeFollowerState)
import qualified Cardano.Node.Types                            as NodeServer
import           Cardano.Wallet.Mock                           (MockWalletState, followerID)
import qualified Cardano.Wallet.Mock                           as WalletServer
import           Control.Concurrent.MVar                       (MVar, newMVar)
import           Control.Lens                                  (makeLenses, view, zoom)
import           Control.Monad                                 (void)
import           Control.Monad.Freer                           (Eff, interpret, interpretM, runM)
import           Control.Monad.Freer.Error                     (Error, handleError, throwError)
import           Control.Monad.Freer.Extra.Log                 (Log, logDebug, logInfo)
import           Control.Monad.Freer.Extra.State               (assign, use)
import           Control.Monad.Freer.Extras                    (errorToMonadError, handleZoomedError, handleZoomedState,
                                                                stateToMonadState)
import           Control.Monad.Freer.State                     (State, runState)
import           Control.Monad.Freer.Writer                    (Writer)
import           Control.Monad.IO.Class                        (MonadIO, liftIO)
import           Control.Monad.Logger                          (LoggingT, MonadLogger, logDebugN, logInfoN,
                                                                runStderrLoggingT)
import           Control.Monad.State                           (MonadState, StateT (StateT), execStateT, runStateT)
import           Data.Aeson                                    as JSON
import           Data.Aeson.Types                              as JSON
import           Data.Bifunctor                                (first)
import           Data.Foldable                                 (traverse_)
import           Data.Text                                     (Text)
import qualified Data.Text                                     as Text
import           Eventful                                      (commandStoredAggregate, getLatestStreamProjection,
                                                                streamEventEvent)
import           Eventful.Store.Memory                         (EventMap, emptyEventMap, stateEventStoreReader,
                                                                stateEventStoreWriter, stateGlobalEventStoreReader)
import           Language.Plutus.Contract.Resumable            (ResumableError)
import           Language.Plutus.Contract.Servant              (initialResponse, runUpdate)
import qualified Language.PlutusTx.Coordination.Contracts.Game as Contracts.Game
import           Ledger                                        (Address)
import qualified Ledger
import           Ledger.AddressMap                             (UtxoMap)
import qualified Ledger.AddressMap                             as AM
import           Plutus.SCB.Command                            ()
import           Plutus.SCB.Core
import           Plutus.SCB.Effects.Contract                   (ContractEffect)
import           Plutus.SCB.Effects.EventLog                   (EventLogEffect)
import           Plutus.SCB.Effects.UUID                       (UUIDEffect)
import           Plutus.SCB.Events                             (ChainEvent)
import           Plutus.SCB.Query                              (pureProjection)
import           Plutus.SCB.Types                              (SCBError (ContractCommandError, ContractNotFound, OtherError),
                                                                _WalletError)
import           Plutus.SCB.Utils                              (abbreviate, tshow)
import           Test.QuickCheck.Instances.UUID                ()

import           Wallet.API                                    (WalletAPIError, addSignatures, ownOutputs, ownPubKey,
                                                                startWatching, submitTxn, updatePaymentWithChange,
                                                                watchedAddresses)
import           Wallet.Effects                                (ChainIndexEffect, NodeClientEffect,
                                                                SigningProcessEffect, WalletEffect)
import           Wallet.Emulator.Chain                         (ChainEffect, ChainState)
import           Wallet.Emulator.ChainIndex                    (ChainIndexControlEffect)
import           Wallet.Emulator.NodeClient                    (NodeControlEffect)
import           Wallet.Emulator.SigningProcess                (SigningProcess, SigningProcessControlEffect)
import qualified Wallet.Emulator.SigningProcess                as SP
import           Wallet.Emulator.Wallet                        (Wallet (..))

data TestState =
    TestState
        { _eventStore     :: EventMap ChainEvent
        , _walletState    :: WalletServer.MockWalletState
        , _nodeState      :: MVar NodeServer.AppState
        , _signingProcess :: SigningProcess
        }

makeLenses 'TestState

initialTestState :: MonadIO m => m TestState
initialTestState =
    liftIO $ do
        let _eventStore = emptyEventMap
        -- ^ Set up the event log.
        -- Set up the node.
        _nodeState <- liftIO $ newMVar NodeServer.initialAppState
        -- Set up the wallet.
        let _walletState = WalletServer.initialState
        let _signingProcess = SP.defaultSigningProcess (Wallet 1)
        pure TestState {_eventStore, _nodeState, _walletState, _signingProcess}

type TestAppEffects =
        '[ GenRandomTx
        , NodeFollowerEffect
        , ChainEffect
        , WalletEffect
        , UUIDEffect
        , ContractEffect
        , ChainIndexEffect
        , ChainIndexControlEffect
        , NodeClientEffect
        , NodeControlEffect
        , SigningProcessEffect
        , SigningProcessControlEffect
        , State NodeFollowerState
        , State ChainState
        , State AppState
        , State MockWalletState
        , State TestState
        , Writer [ChainEvent]
        , Log
        , EventLogEffect ChainEvent
        , Error WalletAPIError
        , Error SCBError
        , IO
        ]

valueAt :: Address -> Eff TestAppEffects Ledger.Value
valueAt = WalletServer.valueAt

runScenario :: Eff TestAppEffects a -> IO ()
runScenario action = do
    testState <- initialTestState
    result <- runTestApp $ do
                sync
                void action
                events :: [ChainEvent] <-
                    fmap streamEventEvent <$> runGlobalQuery pureProjection
                logDebug "Final Event Stream"
                logDebug "--"
                traverse_ (logDebug . abbreviate 120 . tshow) events
                logDebug "--"
    case result of
        Left err -> error $ show err
        Right _  -> pure ()

runTestApp :: Eff TestAppEffects () -> IO (Either SCBError ())
runTestApp = undefined

sync :: Eff TestAppEffects ()
sync = WalletServer.syncState

getFollowerID :: Eff TestAppEffects FollowerID
getFollowerID = do
    mID <- use (walletState . followerID)
    case mID of
        Just fID -> pure fID
        Nothing  -> throwError $ OtherError "TestApp not initialised correctly!"

fromString :: Either String a -> Either SCBError a
fromString = first (ContractCommandError 0 . Text.pack)

fromResumable :: Either (ResumableError Text) a -> Either SCBError a
fromResumable = first (ContractCommandError 0 . Text.pack . show)
