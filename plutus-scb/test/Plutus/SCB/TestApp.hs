{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeOperators              #-}

-- | A test version of the 'App' stack which runs all operations in memory.
-- No networking, no filesystem.
module Plutus.SCB.TestApp
    ( runScenario
    , sync
    , TestAppEffects
    , getFollowerID
    , valueAt
    ) where

import           Cardano.Node.Follower                         (NodeFollowerEffect, handleNodeFollower)
import qualified Cardano.Node.Follower                         as NodeFollower
import           Cardano.Node.Mock                             (NodeServerEffects)
import qualified Cardano.Node.Mock                             as NodeServer
import           Cardano.Node.RandomTx                         (GenRandomTx, runGenRandomTx)
import           Cardano.Node.Types                            (AppState, FollowerID, NodeFollowerState)
import qualified Cardano.Node.Types                            as NodeServer
import           Cardano.Wallet.Mock                           (MockWalletState, followerID)
import qualified Cardano.Wallet.Mock                           as WalletServer
import           Control.Concurrent.MVar                       (MVar, newMVar)
import           Control.Lens                                  (below, makeLenses, view, zoom)
import           Control.Monad                                 (void)
import           Control.Monad.Freer
import           Control.Monad.Freer.Error                     (Error, handleError, runError, throwError)
import           Control.Monad.Freer.Extra.Log                 (Log, logDebug, logInfo, runStderrLog)
import           Control.Monad.Freer.Extra.State               (assign, use)
import           Control.Monad.Freer.Extras                    (errorToMonadError, handleZoomedError, handleZoomedState,
                                                                handleZoomedWriter, stateToMonadState, writeIntoState)
import           Control.Monad.Freer.State                     (State, runState)
import           Control.Monad.Freer.Writer                    (Writer, runWriter)
import           Control.Monad.IO.Class                        (MonadIO, liftIO)
import           Control.Monad.Logger                          (LoggingT, MonadLogger, logDebugN, logInfoN,
                                                                runStderrLoggingT)
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
import           Plutus.SCB.Effects.Contract                   (ContractEffect (..))
import           Plutus.SCB.Effects.EventLog                   (EventLogEffect, handleEventLog, handleEventLogState)
import           Plutus.SCB.Effects.UUID                       (UUIDEffect, handleUUIDEffect)
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
import           Wallet.Emulator.Chain                         (ChainEffect, ChainState, handleChain)
import qualified Wallet.Emulator.Chain
import           Wallet.Emulator.ChainIndex                    (ChainIndexControlEffect, ChainIndexState,
                                                                handleChainIndex, handleChainIndexControl)
import qualified Wallet.Emulator.ChainIndex
import           Wallet.Emulator.MultiAgent                    (EmulatorEvent, chainEvent, chainIndexEvent,
                                                                walletClientEvent, walletEvent)
import           Wallet.Emulator.NodeClient                    (NodeClientState, NodeControlEffect, handleNodeClient,
                                                                handleNodeControl)
import qualified Wallet.Emulator.NodeClient
import           Wallet.Emulator.SigningProcess                (SigningProcess, SigningProcessControlEffect,
                                                                handleSigningProcess, handleSigningProcessControl)
import qualified Wallet.Emulator.SigningProcess                as SP
import           Wallet.Emulator.Wallet                        (Wallet (..), WalletState)
import qualified Wallet.Emulator.Wallet

data TestState =
    TestState
        { _eventStore        :: EventMap ChainEvent
        , _walletState       :: WalletServer.MockWalletState
        , _nodeState         :: NodeServer.AppState
        , _signingProcess    :: SigningProcess
        , _nodeFollowerState :: NodeFollowerState
        , _chainState        :: ChainState
        , _nodeClientState   :: NodeClientState
        , _chainEventLog     :: [ChainEvent]
        , _emulatorEventLog  :: [EmulatorEvent]
        , _chainIndex        :: ChainIndexState
        }

makeLenses 'TestState

defaultWallet :: Wallet
defaultWallet = WalletServer.activeWallet

initialTestState :: TestState
initialTestState =
    TestState
        { _eventStore = emptyEventMap
        , _nodeState = NodeServer.initialAppState
        , _walletState = WalletServer.initialState
        , _signingProcess = SP.defaultSigningProcess WalletServer.activeWallet
        , _nodeFollowerState = NodeServer.initialFollowerState
        , _chainState = Wallet.Emulator.Chain.emptyChainState
        , _nodeClientState = Wallet.Emulator.NodeClient.emptyNodeClientState
        , _chainEventLog = []
        , _emulatorEventLog = []
        , _chainIndex = mempty
        }

type TestAppEffects =
        '[ GenRandomTx
        , NodeFollowerEffect
        , WalletEffect
        , UUIDEffect
        , ContractEffect
        , ChainIndexEffect
        , ChainIndexControlEffect
        , NodeClientEffect
        , NodeControlEffect
        , SigningProcessEffect
        , SigningProcessControlEffect
        , ChainEffect
        , EventLogEffect ChainEvent
        , State NodeFollowerState
        , State ChainState
        , State AppState
        , State MockWalletState
        , State SigningProcess
        , State NodeClientState
        , State ChainIndexState
        , State (EventMap ChainEvent)
        , Log
        , Writer [Wallet.Emulator.Wallet.WalletEvent]
        , Writer [Wallet.Emulator.NodeClient.NodeClientEvent]
        , Writer [Wallet.Emulator.ChainIndex.ChainIndexEvent]
        , Writer [Wallet.Emulator.Chain.ChainEvent]
        , Writer [EmulatorEvent]
        , Writer [ChainEvent]
        , Error WalletAPIError
        , Error SCBError
        , State TestState
        , IO
        ]

valueAt :: Address -> Eff TestAppEffects Ledger.Value
valueAt = WalletServer.valueAt

runScenario :: Eff TestAppEffects a -> IO ()
runScenario action = do
    let testState = initialTestState
    result <- runTestApp $ do
                Wallet.Emulator.Chain.processBlock
                sync
                void action
                events :: [ChainEvent] <-
                    fmap streamEventEvent <$> runGlobalQuery pureProjection
                logDebug "Final Event Stream"
                logDebug "--"
                traverse_ (logDebug . abbreviate 120 . tshow) events
                logDebug "--"
    case result of
        (Left err, _) -> error $ show err
        (Right _, _)  -> pure ()

handleContractTest ::
    (Member (Error SCBError) effs)
    => Eff (ContractEffect ': effs)
    ~> Eff effs
handleContractTest = interpret $ \case
    InvokeContract (InitContract "game") ->
        either throwError pure $ do
            value <- fromResumable $ initialResponse Contracts.Game.game
            fromString $ JSON.eitherDecode (JSON.encode value)
    InvokeContract (UpdateContract "game" payload) ->
        either throwError pure $ do
            request <- fromString $ JSON.parseEither JSON.parseJSON payload
            value <- fromResumable $ runUpdate Contracts.Game.game request
            fromString $ JSON.eitherDecode (JSON.encode value)
    InvokeContract (InitContract contractPath) ->
        throwError $ ContractNotFound contractPath
    InvokeContract (UpdateContract contractPath _) ->
        throwError $ ContractNotFound contractPath

runTestApp :: Eff TestAppEffects () -> IO (Either SCBError (), TestState)
runTestApp =
    runM
    . runState initialTestState
    . runError
    . interpret (handleZoomedError _WalletError)
    . interpret (writeIntoState chainEventLog)
    . interpret (writeIntoState emulatorEventLog)
    . interpret (handleZoomedWriter (below chainEvent))
    . interpret (handleZoomedWriter (below (chainIndexEvent defaultWallet)))
    . interpret (handleZoomedWriter (below (walletClientEvent defaultWallet)))
    . interpret (handleZoomedWriter (below (walletEvent defaultWallet)))
    . runStderrLog
    . interpret (handleZoomedState eventStore)
    . interpret (handleZoomedState chainIndex)
    . interpret (handleZoomedState nodeClientState)
    . interpret (handleZoomedState signingProcess)
    . interpret (handleZoomedState walletState)
    . interpret (handleZoomedState nodeState)
    . interpret (handleZoomedState chainState)
    . interpret (handleZoomedState nodeFollowerState)
    . handleEventLogState
    . handleChain
    . handleSigningProcessControl
    . handleSigningProcess
    . handleNodeControl
    . handleNodeClient
    . handleChainIndexControl
    . handleChainIndex
    . handleContractTest
    . handleUUIDEffect
    . WalletServer.handleWallet
    . handleNodeFollower
    . runGenRandomTx

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
