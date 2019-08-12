{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators    #-}
module Language.Plutus.Contract.Effects.WatchAddress where

import           Control.Lens                               (at, (^.))
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
import           Language.Plutus.Contract.Request           (Contract, mkRequest)

type AddrReq = "interesting addresses" .== Set Address
type AddrResp = "address change" .== (Address, Tx)

-- | Wait for the next transaction that changes an address.
nextTransactionAt :: Address -> Contract AddrResp AddrReq Tx
nextTransactionAt addr =
  let s = Set.singleton addr in
  mkRequest s $ \(addr', tx) -> if addr == addr' then Just tx else Nothing

-- | Watch an address until the given slot, then return all known outputs
--   at the address.
watchAddressUntil :: Address -> Slot -> Contract (SlotResp .\/ AddrResp) (SlotReq .\/ AddrReq) AddressMap
watchAddressUntil a = collectUntil AM.updateAddresses (AM.addAddress a mempty) (nextTransactionAt a)

-- | Watch an address for changes, and return the outputs
--   at that address when the total value at the address
--   has surpassed the given value.
fundsAtAddressGt :: Address -> Value -> Contract AddrResp AddrReq AddressMap
fundsAtAddressGt addr' vl = loopM go mempty where
    go cur = do
        delta <- AM.fromTxOutputs <$> nextTransactionAt addr'
        let cur' = cur <> delta
            presentVal = fromMaybe mempty (AM.values cur' ^. at addr')
        if presentVal `V.gt` vl
        then pure (Right cur') else pure (Left cur')
