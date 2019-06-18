{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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
    ) where

import           Control.Monad.State                  (MonadState)
import qualified Data.Aeson                           as Aeson
import qualified Data.Map                             as Map
import           Data.Semigroup                       (Min (..), Option (..))
import qualified Data.Set                             as Set
import           GHC.Generics                         (Generic)

import           Language.Plutus.Contract.Transaction (UnbalancedTx)
import           Language.Plutus.Contract.TxId
import           Ledger.Slot                          (Slot (..))
import           Ledger.Tx                            (Address)

data Hook a =
    TxHook UnbalancedTx
    | AddrHook Address
    | SlotHook Slot
    | EndpointHook String a -- a is the schema. In the future it will be a type-level Map Symbol GraphQLSchema or whatever (the Symbol being the endpoint name), and the String parameter can go.
    deriving Functor

txHook :: UnbalancedTx -> Hook a
txHook = TxHook

addrHook :: Address -> Hook a
addrHook = AddrHook

slotHook :: Slot -> Hook a
slotHook = SlotHook

endpointHook :: String -> Hook ()
endpointHook s = EndpointHook s ()

-- | Things that a contract can be waiting for.
data Hooks = Hooks
        { _transactions    :: Map.Map UnbalancedTxId UnbalancedTx
        , _addresses       :: Set.Set Address
        , _nextSlot        :: Maybe Slot
        , _activeEndpoints :: Set.Set String
        }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (Aeson.FromJSON, Aeson.ToJSON)

instance Semigroup Hooks where
    l <> r = Hooks
                { _transactions       = _transactions l <> _transactions r
                , _addresses          = _addresses l <> _addresses r
                , _nextSlot           = fmap getMin $ getOption $ Option (Min <$> _nextSlot l) <> Option (Min <$> _nextSlot r)
                , _activeEndpoints    = _activeEndpoints l <> _activeEndpoints r
                }

instance Monoid Hooks where
    mappend = (<>)
    mempty = Hooks mempty mempty Nothing mempty

hooks :: (MonadState UnbalancedTxId m) => Hook a -> m Hooks
hooks = \case
    TxHook utx -> fmap (\i -> mempty { _transactions = Map.singleton i utx}) freshId
    AddrHook a -> pure $ mempty { _addresses = Set.singleton a }
    SlotHook sl -> pure $ mempty { _nextSlot = Just sl }
    EndpointHook n _ -> pure $ mempty { _activeEndpoints = Set.singleton n }
