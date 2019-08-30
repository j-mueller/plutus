{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE MonoLocalBinds      #-}
{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}
module Language.Plutus.Contract.Effects.WriteTx where

import           Data.Row
import           Language.Plutus.Contract.Events  (Event (..), First, Hooks (..), Second)
import           Language.Plutus.Contract.Request as Req
import           Language.Plutus.Contract.Tx      (UnbalancedTx)

type TxPrompt s =
    ( HasType "tx" () (First s)
    , HasType "tx" [UnbalancedTx] (Second s)
    , ContractRow s)

type WriteTxSchema = "tx" .== ((), [UnbalancedTx])

--  | Send an unbalanced transaction to the wallet.
writeTx :: forall s. TxPrompt s => UnbalancedTx -> Contract s ()
writeTx t = request @"tx" @_ @_ @s [t]

event
  :: forall s. (HasType "tx" () (First s), AllUniqueLabels (First s))
  => Event s
event = Event (IsJust #tx ())

transactions
  :: forall s. ( HasType "tx" [UnbalancedTx] (Second s) )
   => Hooks s
   -> [UnbalancedTx]
transactions (Hooks r) = r .! #tx
