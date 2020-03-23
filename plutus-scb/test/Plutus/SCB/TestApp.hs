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
    , TestApp
    , getFollowerID
    , valueAt
    ) where

import qualified Cardano.Node.Follower                         as NodeFollower
import           Cardano.Node.Mock                             (NodeServerEffects)
import qualified Cardano.Node.Mock                             as NodeServer
import           Cardano.Node.Types                            (FollowerID)
import qualified Cardano.Node.Types                            as NodeServer
import           Cardano.Wallet.Mock                           (followerID)
import qualified Cardano.Wallet.Mock                           as WalletServer
import           Control.Concurrent.MVar                       (MVar, newMVar)
import           Control.Lens                                  (assign, makeLenses, use, view, zoom)
import           Control.Monad                                 (void)
import           Control.Monad.Except                          (ExceptT, MonadError, runExceptT, throwError)
import           Control.Monad.Freer                           (Eff, interpret, interpretM, runM)
import           Control.Monad.Freer.Extras                    (errorToMonadError, handleZoomedError, handleZoomedState,
                                                                stateToMonadState)
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
import           Plutus.SCB.Events                             (ChainEvent)
import           Plutus.SCB.Query                              (pureProjection)
import           Plutus.SCB.Types                              (SCBError (ContractCommandError, ContractNotFound, OtherError),
                                                                _WalletError)
import           Plutus.SCB.Utils                              (abbreviate, tshow)
import           Test.QuickCheck.Instances.UUID                ()

import           Wallet.API                                    (addSignatures, ownOutputs, ownPubKey, startWatching,
                                                                submitTxn, updatePaymentWithChange, watchedAddresses)
import           Wallet.Emulator.SigningProcess                (SigningProcess)
import qualified Wallet.Emulator.SigningProcess                as SP
import           Wallet.Emulator.Wallet                        (Wallet (..))

data TestState =
    TestState
        { _eventStore     :: EventMap ChainEvent
        , _walletState    :: WalletServer.State
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

valueAt :: Address -> Eff TestAppEffects Ledger.Value
valueAt address = WalletServer.valueAt address

type TestAppEffects = '[IO]

runScenarioEff :: Eff TestAppEffects a -> IO ()
runScenarioEff action = do
    testState <- initialTestState
    result <-runTestApp $ do
                sync
                void action
                events :: [ChainEvent] <-
                    fmap streamEventEvent <$> runGlobalQuery pureProjection
                logDebugN "Final Event Stream"
                logDebugN "--"
                traverse_ (logDebugN . abbreviate 120 . tshow) events
                logDebugN "--"
    case result of
        Left err -> error $ show err
        Right _  -> pure ()

runTestApp :: Eff TestAppEffects () -> IO ()
runTestApp = undefined

sync :: Eff TestAppEffects ()
sync =
    use walletState >>= execStateT WalletServer.syncState >>= assign walletState

getFollowerID :: Eff TestAppEffects FollowerID
getFollowerID = do
    mID <- use (walletState . followerID)
    case mID of
        Just fID -> pure fID
        Nothing  -> throwError $ OtherError "TestApp not initialised correctly!"

runChainEffects :: Eff (NodeServerEffects IO) a -> Eff TestAppEffects a
runChainEffects action =
    TestApp . zoom nodeState . StateT $ \stateMVar -> do
        result <- NodeServer.processChainEffects stateMVar action
        pure (result, stateMVar)

fromString :: Either String a -> Either SCBError a
fromString = first (ContractCommandError 0 . Text.pack)

fromResumable :: Either (ResumableError Text) a -> Either SCBError a
fromResumable = first (ContractCommandError 0 . Text.pack . show)
