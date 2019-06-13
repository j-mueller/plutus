{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
module Language.Plutus.Contract.Transaction(
      UnbalancedTx(..)
    , unbalancedTx
    ) where


import qualified Data.Aeson   as Aeson
import           GHC.Generics (Generic)

import           Ledger          (PubKey)
import qualified Ledger          as L
import           Ledger.Slot     (SlotRange)
import           Ledger.Value    as V
import qualified Ledger.Interval as I

-- | An unsigned and potentially unbalanced transaction, as produced by
--   a contract endpoint. See note [Unbalanced transactions]
data UnbalancedTx = UnbalancedTx
        { utxInputs             :: [L.TxIn]
        , utxOutputs            :: [L.TxOut]
        , utxForge              :: V.Value
        , utxRequiredSignatures :: [PubKey]
        , utxValidityRange      :: SlotRange
        }
        deriving stock (Eq, Show, Generic)
        deriving anyclass (Aeson.FromJSON, Aeson.ToJSON)

-- | Make an unbalanced transaction that does not forge any value.
unbalancedTx :: [L.TxIn] -> [L.TxOut] -> UnbalancedTx
unbalancedTx ins outs = UnbalancedTx ins outs V.zero [] I.always


{- note [Unbalanced transactions]

To turn an 'UnbalancedTx' into a valid transaction that can be submitted to the
network, the contract backend needs to

* Balance it.
  If the total value of `utxInputs` + the `txForge` field is
  greater than the total value of `utxOutput`, then one or more public key
  outputs need to be added. How many and what addresses they are is up
  to the wallet (probably configurable).
  If the total balance `utxInputs` + the `txForge` field is less than
  the total value of `utxOutput`, then one or more public key inputs need
  to be added (and potentially some outputs for the change)

* Compute fees.
  Once the final size of the transaction is known, the fees for the transaction
  can be computed. The transaction fee needs to be paid for with additional
  inputs so I assume that this step and the previous step will be combined.

  Also note that even if the 'UnbalancedTx' that we get from the contract
  endpoint happens to be balanced already, we still need to add fees to it. So
  we can't skip the balancing & fee computation step.

* Sign it.
  The signing process needs to provide signatures for all public key
  inputs in the balanced transaction, and for all public keys in the
  `utxRequiredSignatures` field.

-}