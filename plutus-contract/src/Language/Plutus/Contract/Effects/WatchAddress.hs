{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds   #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators    #-}
module Language.Plutus.Contract.Effects.WatchAddress where

import           Control.Lens                               (at, (^.))
import           Data.Map                                   (Map)
import qualified Data.Map                                   as Map
import           Data.Maybe                                 (fromMaybe)
import           Data.Row
import           Data.Set                                   (Set)
import qualified Data.Set                                   as Set
import           Language.Plutus.Contract.Util              (loopM)
import           Ledger                                     (Address, Slot, Value)
import           Ledger.AddressMap                          (AddressMap)
import qualified Ledger.AddressMap                          as AM
import           Ledger.Tx                                  (Tx)
import qualified Ledger.Value                               as V

import           Language.Plutus.Contract.Effects.AwaitSlot
import           Language.Plutus.Contract.Events            (Event (..), Hooks (..))
import           Language.Plutus.Contract.Request           (Contract, requestMaybe, ContractRow)

type AddressPrompt i o =
    ( HasType "address" (Set Address) o
    , HasType "address" (Address, Tx) i
    , ContractRow i o)

type AddressIn = "address" .== (Address, Tx)
type AddressOut = "address" .== Set Address

-- | Wait for the next transaction that changes an address.
nextTransactionAt :: AddressPrompt i o => Address -> Contract i o Tx
nextTransactionAt addr =
    let s = Set.singleton addr
        check :: (Address, Tx) -> Maybe Tx
        check (addr', tx) = if addr == addr' then Just tx else Nothing
    in
    requestMaybe @"address" s check

-- | Watch an address until the given slot, then return all known outputs
--   at the address.
watchAddressUntil 
    :: ( SlotPrompt i o
       , AddressPrompt i o
       )
    => Address
    -> Slot
    -> Contract i o AddressMap
watchAddressUntil a = collectUntil AM.updateAddresses (AM.addAddress a mempty) (nextTransactionAt a)

-- | Watch an address for changes, and return the outputs
--   at that address when the total value at the address
--   has surpassed the given value.
fundsAtAddressGt :: AddressPrompt i o => Address -> Value -> Contract i o AddressMap
fundsAtAddressGt addr' vl = loopM go mempty where
    go cur = do
        delta <- AM.fromTxOutputs <$> nextTransactionAt addr'
        let cur' = cur <> delta
            presentVal = fromMaybe mempty (AM.values cur' ^. at addr')
        if presentVal `V.gt` vl
        then pure (Right cur') else pure (Left cur')

events
    :: forall i.
       ( HasType "address" (Address, Tx) i
       , AllUniqueLabels i
       )
    => AddressMap
    -> Tx
    -> Map Address (Event i)
events utxo tx =
    Map.fromSet
        (\addr -> Event $ IsJust (Label @"address") (addr, tx))
        (AM.addressesTouched utxo tx)

addresses
    :: forall i.
    ( HasType "address" (Set Address) i)
    => Hooks i
    -> Set Address
addresses (Hooks r) = r .! Label @"address"
