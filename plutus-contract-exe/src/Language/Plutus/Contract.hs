{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Language.Plutus.Contract(
    PlutusContract
    , nextTransactionAt
    , watchAddressUntil
    , endpoint
    , writeTx
    , fundsAtAddressGt
    , slotGeq
    , selectEither
    , both
    , until
    , when
    , timeout
    , between
    , collectUntil
    , finally
    ) where

import           Control.Applicative                  (Alternative (..))
import           Control.Lens                         hiding (both)
import           Control.Monad.Prompt                 (MonadPrompt (..))
import           Data.Aeson                           (FromJSON)
import qualified Data.Aeson                           as Aeson
import           Data.Maybe                           (fromMaybe)

import           Language.Plutus.Contract.Contract    as Contract
import           Language.Plutus.Contract.Event       as Event hiding (endpoint)
import           Language.Plutus.Contract.Hooks       as Hooks
import           Language.Plutus.Contract.RequestId   (RequestId)
import           Language.Plutus.Contract.Transaction as Transaction

import           Ledger.AddressMap                    (AddressMap)
import qualified Ledger.AddressMap                    as AM
import           Ledger.Slot                          (Slot)
import           Ledger.Tx                            (Address, Tx)
import           Ledger.Value                         (Value)
import qualified Ledger.Value                         as V

import           Prelude                              hiding (until)

type PlutusContract m = (Monad m, Alternative m, MonadPrompt (Hook ()) Event m)

-- | Watch an 'Address', returning the next transaction that changes it
nextTransactionAt :: PlutusContract m => Address -> m Tx
nextTransactionAt a = do
    i <- prompt (Hooks.addrHook a)
    case i of
        LedgerUpdate a' tx
            | a' == a -> pure tx
        _ -> empty

-- | Watch an address until the given slot, then return all known outputs
--   at the address.
watchAddressUntil :: PlutusContract m => Address -> Slot -> m AM.AddressMap
watchAddressUntil a = collectUntil AM.updateAddresses (AM.addAddress a mempty) (nextTransactionAt a)

-- | Monadic version of `<*`
finally :: Monad m => m a -> m b -> m a
finally a b = do
    a' <- a
    _ <- b
    return a'

-- | Expose an endpoint, returning the data that was entered
endpoint :: forall a. forall m. (PlutusContract m, FromJSON a) => String -> m a
endpoint nm = do
    i <- prompt (Hooks.endpointHook nm)
    case i of
        Endpoint nm' vl
            | nm' == nm -> case Aeson.fromJSON vl of
                Aeson.Success r -> pure r
                _               -> empty -- TODO: Report error somewhere
        _ -> empty


-- | Produce an unbalanced transaction.
--
--   TODO: returning a 'RequestID' that can be used to query its status (TBD)
writeTx :: PlutusContract m => UnbalancedTx -> m ()
writeTx t = do
    i <- prompt (Hooks.txHook t)
    case i of
        TxSubmission -> pure ()
        _            -> empty

-- | Watch an address for changes, and return the outputs
--   at that address when the total value at the address
--   has surpassed the given value.
fundsAtAddressGt :: PlutusContract m => Address -> Value -> m AddressMap
fundsAtAddressGt addr' vl = loopM go mempty where
    go cur = do
        delta <- AM.fromTxOutputs <$> nextTransactionAt addr'
        let cur' = cur <> delta
            presentVal = fromMaybe mempty (AM.values cur' ^. at addr')
        if presentVal `V.gt` vl
        then pure (Left cur') else pure (Right cur')

-- | Wait until a slot number has been reached
-- slotGeq :: (PlutusContract m) => Slot -> m Slot
slotGeq sl = do
    i <- prompt (Hooks.slotHook sl)
    case i of
        SlotChange sl'
            | sl' >= sl -> pure sl'
        _ -> empty

-- | Run a contract until the given slot has been reached.
until :: (PlutusContract m) => m a -> Slot -> m (Maybe a)
until c sl = fmap (either (const Nothing) Just) (selectEither (slotGeq sl) c)

-- | Run a contract when the given slot has been reached.
when :: (PlutusContract m) => Slot -> m a -> m a
when s c = slotGeq s >> c

-- | Run a contract until the given slot has been reached.
--   @timeout = flip until@
timeout :: PlutusContract m =>  Slot -> m a -> m (Maybe a)
timeout = flip until

-- | Wait until the first slot is reached, then run the contract until
--   the second slot is reached.
between :: PlutusContract m => Slot -> Slot -> m a -> m (Maybe a)
between a b = timeout b . when a

-- | Repeatedly run a contract until the slot is reached, then
--   return the last result.
collectUntil :: PlutusContract m => (a -> b -> b) -> b -> m a -> Slot -> m b
collectUntil f b con s = foldMaybe f b (timeout s con)
