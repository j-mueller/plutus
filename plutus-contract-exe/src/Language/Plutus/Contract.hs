{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
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

    * 1 route per contract endpoint (as defined by contract author)
    * 1 route for submitting 'LedgerUpdate' events

    NOTE: With this design, there are two classes of input events, only one of
          which has a proper data type: Input events from the ledger (in the
          form of 'LedgerUpdate') and input events from the user (in the form
          of HTTP endpoints).

          In my opinion it would be cleaner to add a

          'ContractEndpoint EndpointArgs'

          constructor to 'LedgerUpdate' (and rename the type) that captures all
          contract endpoints activated by the user. I think this will make
          testing a lot easier. But we need to think about the 'EndpointArgs'
          type and how it maps to the actual contract endpoints.
-}
module Language.Plutus.Contract(
    ContractOut(..),
    LedgerUpdate(..)
    ) where

import qualified Data.Aeson   as Aeson
import           Data.Text    (Text)
import           GHC.Generics (Generic)

import qualified Ledger.Types as T

-- | Events that are produced by contract endpoints.
data ContractOut =
      SubmitTransaction T.Tx
      -- ^ Produce a transaction. The transaction is unsigned and may or may
      --   not be balanced. If it is not balanced then the wallet is expected to
      --   balance it by
      --   (a) adding public key inputs if the transaction produces more than
      --       it spends
      --   (b) adding public key outputs if the transaction spends more than it
      --       produces

      | StartWatching T.Address
      -- ^ Start watching an address. This adds the address to the set of
      --   "interesting addresses" of this contract instance.

      | ContractError Text
      -- ^ An error occurred during contract execution.
      --   NOTE: Should we also set the appropriate HTTP status code?

      | ContractFinished
      -- ^ Execution of the contract has ended. No further ledger updates are
      --   required and no user actions are possible. All triggers associated
      --   with this contract instance can be deleted.

      deriving stock (Eq, Ord, Show, Generic)
      deriving anyclass (Aeson.FromJSON, Aeson.ToJSON)

-- | Events that inform the contract about changes to the ledger state.
data LedgerUpdate =
    OutputSpent T.TxOutRef T.Tx
    -- ^ An output from one of the interesting addresses was spent. Includes
    --   the transaction that spent the output.
    --   The transaction *spends* the UTXO referenced by 'T.TxOutRef'.

    | OutputAdded T.TxOutRef T.Tx
    -- ^ An output was produced to one of the interesting addresses.
    --   The transaction *produces* the UTXO referenced by 'T.TxOutRef'

    -- NOTE: A transaction that spends an output from an interesting address and
    --       produces a new output to that address will trigger two events:
    --       'OutputSpent' and 'OutputAdded' (with the same 'T.Tx'
    --       argument but with different 'T.TxOutRef' arguments).
    --
    --       TODO: Does it make sense to have a 3rd event type for this
    --       situation?

    | SlotChange T.Slot
    -- ^ The current slot has changed.

    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (Aeson.FromJSON, Aeson.ToJSON)
