{-# LANGUAGE FlexibleContexts #-}
-- | Balance, sign and submit `UnbalancedTx` values
module Language.Plutus.Contract.Wallet where

import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.Reader                 (MonadReader)
import           Data.Map                             (Map)
import qualified Data.Map                             as Map
import           Language.Plutus.Contract.Transaction (UnbalancedTx)
import qualified Language.Plutus.Contract.Transaction as T
import           Ledger.Tx                            (Tx, TxOutRef, TxOut)
import           Wallet.API                           (WalletAPIError)

balance
    :: ( MonadReader (Map TxOutRef TxOut) m
       , MonadError WalletAPIError m)
    => UnbalancedTx
    -> m Tx
balance utx = do
    let b = T.computeBalance utx
    undefined
