{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase         #-}
module Language.Plutus.Contract.Event(
      Event(..)
    -- * Produce events
    , updateLedger
    , changeSlot
    , endpoint
    , txSubmission
    , txEvents
    -- * Consume events
    , ledgerUpdate
    , slotChange
    , endpointEvent
    ) where

import qualified Data.Aeson        as Aeson
import           Data.Map          (Map)
import qualified Data.Map          as Map
import           GHC.Generics      (Generic)

import           Ledger.AddressMap (AddressMap)
import qualified Ledger.AddressMap as AM
import           Ledger.Slot       (Slot)
import           Ledger.Tx         (Address, Tx)

data Event =
    LedgerUpdate Address Tx
    | TxSubmission -- TODO: add more events about specific transactions (namely, tx submitted, tx rejected, tx rolled back, etc.)
    | SlotChange Slot
    | Endpoint String Aeson.Value
    deriving stock (Eq, Show, Generic)
    deriving anyclass (Aeson.FromJSON, Aeson.ToJSON) -- TODO: Make sure this can be created easily by app platform

txEvents :: AddressMap -> Tx -> Map Address Event
txEvents utxo t = Map.fromSet (`updateLedger` t) (AM.addressesTouched utxo t)

updateLedger :: Address -> Tx -> Event
updateLedger = LedgerUpdate

changeSlot :: Slot -> Event
changeSlot = SlotChange

endpoint :: String -> Aeson.Value -> Event
endpoint = Endpoint

txSubmission :: Event
txSubmission = TxSubmission

ledgerUpdate :: Event -> Maybe (Address, Tx)
ledgerUpdate = \case
    LedgerUpdate a t -> Just (a, t)
    _ -> Nothing

slotChange :: Event -> Maybe Slot
slotChange = \case
    SlotChange sl -> Just sl
    _ -> Nothing

endpointEvent :: Event -> Maybe (String, Aeson.Value)
endpointEvent = \case
    Endpoint s v -> Just (s, v)
    _ -> Nothing

{-
    Event-based interface between contract executables and the app platform and
    (by extension) the wallet.

    Two types of events are defined:

    1. 'ContractOut'. Events produced by the contract for consumption by app
       platform and wallet. Includes transactions and instructions to start
       watching interesting addresses on the ledger.

    2. 'LedgerUpdate'. Events that inform the contract about changes to the
       ledger state.

    Contracts will offer an HTTP interface with the following routes.

    * 1 route "initialise" to get the list of endpoints and interesting
      addresses at the start of the instance
    * 1 route "update" to advance the contract to the next state given some
      input (user interaction or blockchain)

-}
