{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE ExplicitNamespaces         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TypeOperators              #-}

module Wallet.Emulator.Http
  ( app
  , initialState
  , API
  ) where

import           Control.Concurrent.STM     (STM, TVar, atomically, modifyTVar, newTVar, readTVar, readTVarIO,
                                             writeTVar)
import           Control.Monad              (void)
import           Control.Monad.Error.Class  (MonadError)
import           Control.Monad.Except       (ExceptT, liftEither, runExceptT)
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Control.Monad.Reader       (MonadReader, ReaderT, asks, runReaderT)
import           Control.Monad.State        (runState)
import           Control.Natural            (type (~>))
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Map                   as Map
import           Data.Maybe                 (catMaybes)
import           Data.Proxy                 (Proxy (Proxy))
import           Data.Set                   (Set)
import qualified Data.Set                   as Set
import           Lens.Micro                 (over, set, to)
import           Lens.Micro.Extras          (view)
import           Servant                    (Application, Handler, ServantErr (errBody), Server, err404, err500,
                                             hoistServer, serve, throwError)
import           Servant.API                ((:<|>) ((:<|>)), (:>), Capture, Get, JSON, NoContent (NoContent), Post,
                                             Put, ReqBody)
import qualified Wallet.API                 as WAPI
import           Wallet.Emulator.Types      (AssertionError, EmulatedWalletApi, EmulatorState (emWalletState),
                                             Notification (BlockHeight, BlockValidated), Trace, Wallet, WalletState,
                                             chain, emTxPool, emptyEmulatorState, emptyWalletState, process, txPool,
                                             validateEm, validationData, walletAction, walletStates)

import qualified Wallet.Emulator.Types      as Types
import           Wallet.UTXO                (Block, Height, Tx, TxIn', TxOut', ValidationData, Value)

type WalletAPI
   = "wallets" :> Get '[ JSON] [Wallet]
     :<|> "wallets" :> Capture "walletid" Wallet :> Get '[ JSON] Wallet
     :<|> "wallets" :> ReqBody '[ JSON] Wallet :> Post '[ JSON] NoContent
     :<|> "wallets" :> Capture "walletid" Wallet :> "payments" :> ReqBody '[ JSON] Value :> Post '[ JSON] ( Set TxIn'
                                                                                                          , TxOut')
     :<|> "wallets" :> Capture "walletid" Wallet :> "pay-to-public-key" :> ReqBody '[ JSON] Value :> Post '[ JSON] TxOut'
     :<|> "wallets" :> Capture "walletid" Wallet :> "transactions" :> ReqBody '[ JSON] Tx :> Post '[ JSON] ()
     :<|> "wallets" :> "transactions" :> Get '[ JSON] [Tx]

type ControlAPI
   = "emulator" :> ("blockchain-actions" :> Get '[ JSON] [Tx]
                    :<|> "validation-data" :> ReqBody '[ JSON] ValidationData :> Put '[ JSON] ()
                    :<|> "wallets" :> Capture "walletid" Wallet :> "notifications" :> "block-validation" :> ReqBody '[ JSON] Block :> Post '[ JSON] ()
                    :<|> "wallets" :> Capture "walletid" Wallet :> "notifications" :> "block-height" :> ReqBody '[ JSON] Height :> Post '[ JSON] ())

type AssertionsAPI
   = "assertions" :> ("own-funds-eq" :> Capture "walletid" Wallet :> ReqBody '[ JSON] Value :> Post '[ JSON] NoContent
                      :<|> "is-validated-txn" :> ReqBody '[ JSON] Tx :> Post '[ JSON] NoContent)

type API
   = WalletAPI
     :<|> ControlAPI
     :<|> AssertionsAPI

newtype State = State
  { getState :: TVar EmulatorState
  }

wallets ::
     (MonadError ServantErr m, MonadReader State m, MonadIO m) => m [Wallet]
wallets = do
  var <- asks getState
  ws <- liftIO $ readTVarIO var
  pure . Map.keys . emWalletState $ ws

fetchWallet ::
     (MonadError ServantErr m, MonadReader State m, MonadIO m)
  => Wallet
  -> m Wallet
fetchWallet wallet = do
  var <- asks getState
  ws <- liftIO $ readTVarIO var
  if Map.member wallet . emWalletState $ ws
    then pure wallet
    else throwError err404

createWallet :: (MonadReader State m, MonadIO m) => Wallet -> m NoContent
createWallet wallet = do
  var <- asks getState
  let walletState = emptyWalletState wallet
  liftIO . atomically $ modifyTVar var (insertWallet wallet walletState)
  pure NoContent

createPaymentWithChange ::
     (MonadReader State m, MonadIO m, MonadError ServantErr m)
  => Wallet
  -> Value
  -> m (Set.Set TxIn', TxOut')
createPaymentWithChange wallet =
  runWalletAction wallet . WAPI.createPaymentWithChange

payToPublicKey ::
     (MonadReader State m, MonadIO m, MonadError ServantErr m)
  => Wallet
  -> Value
  -> m TxOut'
payToPublicKey wallet = runWalletAction wallet . WAPI.payToPublicKey

submitTxn ::
     (MonadReader State m, MonadIO m, MonadError ServantErr m)
  => Wallet
  -> Tx
  -> m ()
submitTxn wallet = void . runWalletAction wallet . WAPI.submitTxn

runWalletAction ::
     (MonadReader State m, MonadIO m, MonadError ServantErr m)
  => Wallet
  -> EmulatedWalletApi a
  -> m a
runWalletAction wallet action = do
  (_, eRes) <- runTrace . walletAction wallet $ action
  case eRes of
    Left walletAPIError ->
      throwError $ err500 {errBody = BSL.pack . show $ walletAPIError}
    Right v -> pure v

insertWallet :: Wallet -> WalletState -> EmulatorState -> EmulatorState
insertWallet w ws = over walletStates (Map.insert w ws)

getTransactions :: (MonadReader State m, MonadIO m) => m [Tx]
getTransactions = do
  var <- asks getState
  states <- liftIO $ readTVarIO var
  view (txPool . to pure) states

-- | Concrete monad stack for server server
newtype AppM a = AppM
  { unM :: ReaderT State (ExceptT ServantErr IO) a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadReader State
             , MonadIO
             , MonadError ServantErr
             )

runM :: State -> AppM ~> Handler
runM state r = do
  res <- liftIO . runExceptT . flip runReaderT state . unM $ r
  liftEither res

walletHandlers :: State -> Server API
walletHandlers state =
  hoistServer api (runM state) $ walletApi :<|> controlApi :<|> assertionsApi
  where
    walletApi =
      wallets :<|> fetchWallet :<|> createWallet :<|> createPaymentWithChange :<|>
      payToPublicKey :<|>
      submitTxn :<|>
      getTransactions
    controlApi =
      blockchainActions :<|> setValidationData :<|> blockValidated :<|>
      blockHeight
    assertionsApi = assertOwnFundsEq :<|> assertIsValidated

assertOwnFundsEq ::
     (MonadError ServantErr m, MonadReader State m, MonadIO m)
  => Wallet
  -> Value
  -> m NoContent
assertOwnFundsEq wallet value =
  fmap (const NoContent) $ runTrace $ Types.assertOwnFundsEq wallet value

assertIsValidated ::
     (MonadError ServantErr m, MonadReader State m, MonadIO m)
  => Tx
  -> m NoContent
assertIsValidated tx =
  fmap (const NoContent) $ runTrace $ Types.assertIsValidated tx

runTrace ::
     (MonadError ServantErr m, MonadReader State m, MonadIO m) => Trace a -> m a
runTrace trace = do
  var <- asks getState
  res <- liftIO . atomically . runTraceSTM var $ trace
  case res of
    Left e  -> throwError $ err500 {errBody = BSL.pack . show $ e}
    Right a -> pure a

runTraceSTM :: TVar EmulatorState -> Trace a -> STM (Either AssertionError a)
runTraceSTM var trace = do
  es <- readTVar var
  let (res, newState) = runState (runExceptT $ process trace) es
  writeTVar var newState
  pure res

handleNotifications ::
     (MonadReader State m, MonadIO m, MonadError ServantErr m)
  => Wallet
  -> [Notification]
  -> m ()
handleNotifications wallet =
  void . runTrace . Types.walletRecvNotifications wallet

blockValidated ::
     (MonadReader State m, MonadIO m, MonadError ServantErr m)
  => Wallet
  -> Block
  -> m ()
blockValidated wallet block = handleNotifications wallet [BlockValidated block]

blockHeight ::
     (MonadReader State m, MonadIO m, MonadError ServantErr m)
  => Wallet
  -> Height
  -> m ()
blockHeight wallet height = handleNotifications wallet [BlockHeight height]

setValidationData :: (MonadReader State m, MonadIO m) => ValidationData -> m ()
setValidationData vd = do
  var <- asks getState
  liftIO . atomically $ modifyTVar var (set validationData vd)

blockchainActions :: (MonadReader State m, MonadIO m) => m [Tx]
blockchainActions = do
  var <- asks getState
  liftIO . atomically $ blockchainActionsSTM var

blockchainActionsSTM :: TVar EmulatorState -> STM [Tx]
blockchainActionsSTM var = do
  es <- readTVar var
  let processed = validateEm es <$> emTxPool es
      validated = catMaybes processed
      block = validated
      newState = addBlock block . emptyPool $ es
  writeTVar var newState
  pure block
  where
    addBlock block = over chain ((:) block)
    emptyPool = set txPool []

api :: Proxy API
api = Proxy

app :: State -> Application
app state = serve api $ walletHandlers state

initialState :: IO State
initialState = atomically $ State <$> newTVar emptyEmulatorState
