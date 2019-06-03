{-# LANGUAGE FlexibleContexts #-}
-- | Balance  `UnbalancedTx` values using the
--   wallet API
module Language.Plutus.Contract.Wallet(
      balanceWallet
    , balanceTx
    , handleTx
    ) where

import           Control.Lens
import           Control.Monad                        ((>=>))
import           Control.Monad.Except
import           Data.Bifunctor                       (second)
import           Data.Map                             (Map)
import qualified Data.Map                             as Map
import           Data.Maybe                           (fromMaybe)
import qualified Data.Set                             as Set
import           Language.Plutus.Contract.Transaction (UnbalancedTx)
import qualified Language.Plutus.Contract.Transaction as T
import           Ledger.Tx                            (Tx, TxOut, TxOutRef)
import qualified Ledger.Tx                            as Tx
import           Ledger.Value                         (Value)
import qualified Ledger.Value                         as Value
import           Wallet.API                           (MonadWallet, PubKey, WalletAPIError)
import qualified Wallet.API                           as WAPI
import qualified Wallet.Emulator                      as E

-- | Balance an unbalanced transaction in a 'MonadWallet' context.
balanceWallet
    :: (WAPI.MonadWallet m)
    => UnbalancedTx
    -> m Tx
balanceWallet utx = do
    pk <- WAPI.ownPubKey
    addr <- WAPI.watchedAddresses
    let utxo = addr ^. at (Tx.pubKeyAddress pk) . to (fromMaybe mempty)
    balanceTx utxo pk utx

-- | Balance an unbalanced transaction by adding public key inputs
--   and outputs
balanceTx
    :: ( MonadError WalletAPIError m )
    => Map TxOutRef TxOut
    -- ^ Unspent transaction outputs that may be used to balance the
    --   left hand side (inputs) of the transaction.
    -> PubKey
    -- ^ Public key, used to balance the right hand side (outputs) of
    --   the transaction.
    -> UnbalancedTx
    -- ^ The unbalanced transaction
    -> m Tx
balanceTx utxo pk tx =
    let (neg, pos) = Value.split (T.computeBalance tx)
    in addInputs utxo pk neg (addOutputs pk pos (T.toLedgerTx tx))

-- | @addInputs mp pk vl tx@ selects transaction outputs worth at least
--   @vl@ from the UTXO map @mp@ and adds them as inputs to @tx@. A public
--   key output for @pk@ is added containing any leftover change.
addInputs
    :: MonadError WalletAPIError m
    => Map TxOutRef TxOut
    -> PubKey
    -> Value
    -> Tx
    -> m Tx
addInputs mp pk vl tx = do
    (spend, change) <- E.selectCoin (second Tx.txOutValue <$> Map.toList mp) vl
    let

        addTxIns  =
            let ins = Set.fromList (flip Tx.pubKeyTxIn pk . fst <$> spend)
            in over Tx.inputs (Set.union ins)

        addTxOuts = if change == Value.zero
                    then id
                    else addOutputs pk change

    pure $ tx & addTxOuts & addTxIns

addOutputs :: PubKey -> Value -> Tx -> Tx
addOutputs pk vl tx = tx & over Tx.outputs (pko :) where
    pko = Tx.pubKeyTxOut vl pk

-- | Balance an unabalanced transaction, sign it, and submit
--   it to the chain in the context of a wallet. Note that this
--   only attaches a single signature to the transaction before
--   submitting it. Hence the `requiredSignatures` field of the
--   unbalanced transaction is ignored.
handleTx :: MonadWallet m => UnbalancedTx -> m ()
handleTx = balanceWallet >=> WAPI.signTxAndSubmit_
