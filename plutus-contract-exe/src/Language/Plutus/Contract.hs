{-# LANGUAGE ScopedTypeVariables #-}
module Language.Plutus.Contract(
    PlutusContract
    , watchAddress
    , endpoint
    , writeTx
    , fundsAtAddressGt
    , emit
    , slotGeq
    , firstOf
    , select
    , both
    , until
    ) where

import           Control.Lens                         hiding (both, firstOf)
import           Control.Monad                        ((>=>))
import           Data.Aeson                           (FromJSON)
import qualified Data.Aeson                           as Aeson
import           Data.Maybe                           (fromMaybe)

import           Language.Plutus.Contract.Contract    as Contract
import           Language.Plutus.Contract.Event       as Event
import           Language.Plutus.Contract.Transaction as Transaction

import           Ledger.AddressMap                    (AddressMap)
import qualified Ledger.AddressMap                    as AM
import           Ledger.Slot                          (Slot)
import           Ledger.Tx                            (Address, Tx)
import           Ledger.Value                         (Value)
import qualified Ledger.Value                         as V

import           Prelude                              hiding (until)

type PlutusContract a = Contract Event Step a

-- | Watch an 'Address', returning the next transaction that changes it
watchAddress :: Address -> PlutusContract Tx
watchAddress a = await (Event.addr a) (ledgerUpdate >=> check) where
    check (a', t)
        | a == a' = Just t
        | otherwise = Nothing

-- | Expose an endpoint, returning the data that was entered
endpoint :: forall a. FromJSON a => String -> PlutusContract a
endpoint nm = await (Event.endpointName nm) (endpointEvent >=> uncurry dec) where
    dec :: String -> Aeson.Value -> Maybe a
    dec nm' vl
        | nm' == nm =
            case Aeson.fromJSON vl of
                Aeson.Success r -> Just r
                _               -> Nothing
        | otherwise = Nothing

-- | Produce an unbalanced transaction
writeTx :: UnbalancedTx -> PlutusContract ()
writeTx = emit . Event.tx

-- | Watch an address for changes, and return the outputs
--   at that address when the total value at the address
--   has surpassed the given value.
fundsAtAddressGt :: Address -> Value -> PlutusContract AddressMap
fundsAtAddressGt addr' vl = loopM go mempty where
    go cur = do
        delta <- AM.fromTxOutputs <$> watchAddress addr'
        let cur' = cur <> delta
            presentVal = fromMaybe mempty (AM.values cur' ^. at addr')
        if presentVal `V.gt` vl
        then pure (Left cur') else pure (Right cur')

-- | Wait until a slot number has been reached
slotGeq :: Slot -> PlutusContract Slot
slotGeq sl = await (Event.slot sl) (slotChange >=> go) where
    go sl'
        | sl' >= sl = Just sl'
        | otherwise = Nothing

-- | Run a contract until the given slot has been reached.
until :: PlutusContract a -> Slot -> PlutusContract (Maybe a)
until c sl = fmap (either (const Nothing) Just) (firstOf (slotGeq sl) c)
