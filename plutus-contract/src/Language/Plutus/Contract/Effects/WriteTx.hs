{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds   #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators    #-}
module Language.Plutus.Contract.Effects.WriteTx where

import           Data.Row

import           Language.Plutus.Contract.Events  (Event (..), Hooks (..))
import           Language.Plutus.Contract.Request as Req
import           Language.Plutus.Contract.Tx      (UnbalancedTx)

type TxPrompt i o =
    ( HasType "tx" () i
    , HasType "tx" [UnbalancedTx] o
    , ContractRow i o)

type WriteTxIn = "tx" .== ()
type WriteTxOut = "tx" .== [UnbalancedTx]

--  | Send an unbalanced transaction to the wallet.
writeTx :: TxPrompt i o => UnbalancedTx -> Contract i o ()
writeTx t = request @"tx" [t]

event
  :: forall i. (HasType "tx" () i, AllUniqueLabels i)
  => Event i
event = Event (IsJust #tx ())

transactions
  :: forall i. ( HasType "tx" [UnbalancedTx] i )
   => Hooks i
   -> [UnbalancedTx]
transactions (Hooks r) = r .! #tx
