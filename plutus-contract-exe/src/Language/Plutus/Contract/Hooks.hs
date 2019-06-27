{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
module Language.Plutus.Contract.Hooks(
    -- * Hooks
      Hook(..)
    , txHook
    , addrHook
    , slotHook
    , endpointHook
    -- * Data about outstanding requests
    , Hooks(..)
    , hooks
    , transactions
    ) where

import           Control.Lens
import           Control.Monad.State                  (MonadState)
import           Control.Monad.Trans.Error
import qualified Data.Aeson                           as Aeson
import qualified Data.Map                             as Map
import           Data.Maybe                           (catMaybes)
import           Data.Semigroup                       (Min (..), Option (..))
import qualified Data.Set                             as Set
import           GHC.Generics                         (Generic)

import           Language.Plutus.Contract.RequestId
import           Language.Plutus.Contract.Transaction (UnbalancedTx)
import           Ledger.Slot                          (Slot (..))
import           Ledger.Tx                            (Address)

data Hook a =
    TxHook UnbalancedTx
    | AddrHook Address
    | SlotHook Slot
    | EndpointHook String a -- a is the schema. In the future it will be a type-level Map Symbol GraphQLSchema or whatever (the Symbol being the endpoint name), and the String parameter can go.
    deriving stock (Eq, Show, Generic, Functor)
    deriving anyclass (Aeson.FromJSON, Aeson.ToJSON)

makePrisms ''Hook

txHook :: UnbalancedTx -> Hook a
txHook = TxHook

addrHook :: Address -> Hook a
addrHook = AddrHook

slotHook :: Slot -> Hook a
slotHook = SlotHook

endpointHook :: String -> Hook ()
endpointHook s = EndpointHook s ()

newtype Hooks = Hooks { unHooks :: Map.Map RequestId (Hook ()) }
    deriving stock (Eq, Show, Generic)
    deriving newtype (Aeson.FromJSON, Aeson.ToJSON, Semigroup, Monoid)

instance Error Hooks where
    noMsg = mempty -- :( It doesn't compile without this instance. I haven't
    -- been able to track down the exact cause yet but it should really
    -- be possible to do it without Error.

transactions :: Hooks -> [(RequestId, UnbalancedTx)]
transactions = catMaybes . fmap (traverse (preview _TxHook)) . Map.toList . unHooks

hooks :: (MonadState RequestId m) => Hook () -> m (Hooks, RequestId)
hooks h = do
    i <- freshId
    pure (Hooks $ Map.singleton i h, i)
