{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators    #-}
module Language.Plutus.Contract(
      Contract
    , ContractRow
    , both
    , selectEither
    , select
    , (>>)
    , (<|>)
    -- * Dealing with time
    , SlotPrompt
    , awaitSlot
    , until
    , when
    , timeout
    , between
    , collectUntil
    -- * Endpoints
    , EndpointPrompt
    , Endpoint
    , endpoint
    -- * Transactions
    , TxPrompt
    , writeTx
    -- * Blockchain events
    , AddressPrompt
    , nextTransactionAt
    , watchAddressUntil
    , fundsAtAddressGt
    -- * Transactions
    , module Tx
    -- * Row-related things
    , HasType
    , BlockchainSchema
    , type (.==)
    , type (.\/)
    , type First
    , type Second
    ) where

import           Control.Applicative                             (Alternative(..))
import           Data.Row                                        (HasType)

import           Language.Plutus.Contract.Effects.AwaitSlot
import           Language.Plutus.Contract.Effects.ExposeEndpoint
import           Language.Plutus.Contract.Effects.WatchAddress
import           Language.Plutus.Contract.Effects.WriteTx
import           Language.Plutus.Contract.Util                   (both, selectEither)

import           Language.Plutus.Contract.Request                (Contract, ContractRow, select)
import           Language.Plutus.Contract.Schema (First, Second)
import           Language.Plutus.Contract.Tx                     as Tx

import           Prelude                                         hiding (until)

import Data.Row

-- | Schema for contracts that can interact with the blockchain (via a wallet)
type BlockchainSchema =
  SlotSchema
  .\/ AddressSchema
  .\/  WriteTxSchema
  
