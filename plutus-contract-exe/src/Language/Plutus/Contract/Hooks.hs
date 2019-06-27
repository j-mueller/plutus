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
import qualified Data.Aeson                           as Aeson
import           Data.Sequence                        (Seq)
import qualified Data.Sequence                        as Seq
import           GHC.Generics                         (Generic)

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

newtype Hooks = Hooks { unHooks :: Seq (Hook ()) }
    deriving stock (Eq, Show, Generic)
    deriving newtype (Aeson.FromJSON, Aeson.ToJSON, Semigroup, Monoid)

transactions :: Hooks -> [UnbalancedTx]
transactions = toListOf (traversed . _TxHook) . unHooks

hooks :: Hook () -> Hooks
hooks = Hooks . Seq.singleton
