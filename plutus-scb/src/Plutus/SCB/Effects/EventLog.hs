{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeOperators    #-}

module Plutus.SCB.Effects.EventLog where

import           Control.Lens
import           Control.Monad.Freer
import           Control.Monad.Freer.Reader
import           Control.Monad.Freer.TH     (makeEffect)
import qualified Control.Monad.IO.Unlift    as Unlift
import qualified Control.Monad.Logger       as MonadLogger
import           Data.Aeson                 (FromJSON, ToJSON)
import qualified Data.UUID                  as UUID
import           Database.Persist.Sqlite    hiding (Connection)
import           Eventful
import           Eventful                   (Aggregate (..), GlobalStreamProjection)
import           Eventful.Store.Sql
import           Eventful.Store.Sqlite      (sqliteEventStoreWriter)

import           Plutus.SCB.Events          (ChainEvent (..))
import           Plutus.SCB.Query           (nullProjection)

newtype Connection =
    Connection (SqlEventStoreConfig SqlEvent JSONString, ConnectionPool)

------------------------------------------------------------
data Source
    = ContractEventSource
    | WalletEventSource
    | UserEventSource
    | NodeEventSource
    deriving (Show, Eq)

-- | Event effects
data EventLogState pjs =
     EventLogState { _elsEvents      :: [ChainEvent]
                   , _elsProjections :: pjs }
makeLenses ''EventLogState

data EventLogEffect event r where
    RefreshProjection :: GlobalStreamProjection state event
                     -> EventLogEffect event (GlobalStreamProjection state event)
    RunCommand :: Aggregate state event command -> Source -> command -> EventLogEffect event [event]
makeEffect ''EventLogEffect

handleEventLog ::
    forall effs event m.
    ( Member (Reader Connection) effs
    , LastMember m effs
    , ToJSON event
    , FromJSON event
    , MonadLogger.MonadLogger m
    , Unlift.MonadUnliftIO m
    )
    => Eff (EventLogEffect event ': effs) ~> Eff effs
handleEventLog = interpret $ \case
    RefreshProjection projection -> do
        (Connection (sqlConfig, connectionPool)) <- ask
        sendM $ do
            let reader =
                    serializedGlobalEventStoreReader jsonStringSerializer $
                    sqlGlobalEventStoreReader sqlConfig
            flip runSqlPool connectionPool $
                getLatestStreamProjection reader projection
    RunCommand aggregate source input -> do
        (Connection (sqlConfig, connectionPool)) <- ask
        sendM $ do
            let reader =
                    serializedVersionedEventStoreReader jsonStringSerializer $
                    sqlEventStoreReader sqlConfig
            let writer =
                    addProcessBus
                        (serializedEventStoreWriter jsonStringSerializer $
                        sqliteEventStoreWriter sqlConfig)
                        reader
            retryOnBusy . flip runSqlPool connectionPool $
                commandStoredAggregate writer reader aggregate (toUUID source) input

runGlobalQuery ::
       Member (EventLogEffect event) effs
    => Projection a (VersionedStreamEvent event)
    -> Eff effs a
runGlobalQuery query =
    fmap streamProjectionState <$> refreshProjection $
    globalStreamProjection query

addProcessBus ::
       Monad m
    => EventStoreWriter m event
    -> VersionedEventStoreReader m event
    -> EventStoreWriter m event
addProcessBus writer reader =
    synchronousEventBusWrapper
        writer
        [ \subwriter _ _ ->
              applyProcessManagerCommandsAndEvents
                  (ProcessManager nullProjection (const []) (const []))
                  subwriter
                  reader
                  ()
        ]


toUUID :: Source -> UUID
toUUID ContractEventSource = UUID.fromWords 0 0 0 2
toUUID WalletEventSource   = UUID.fromWords 0 0 0 2
toUUID UserEventSource     = UUID.fromWords 0 0 0 3
toUUID NodeEventSource     = UUID.fromWords 0 0 0 4
