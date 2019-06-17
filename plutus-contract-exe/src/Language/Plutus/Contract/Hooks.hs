{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Language.Plutus.Contract.Hooks where

import qualified Data.Aeson                           as Aeson
import qualified Data.Map                             as Map
import           Data.Semigroup                       (Min (..), Option (..))
import qualified Data.Set                             as Set
import           GHC.Generics                         (Generic)

import           Language.Plutus.Contract.Balanced
import           Language.Plutus.Contract.Transaction (UnbalancedTx)
import           Ledger.Slot                          (Slot (..))
import           Ledger.Tx                            (Address)

-- | Things that a contract can be waiting for.
data Hooks = Hooks
        { _transactions :: [UnbalancedTx]
        , _addresses    :: Set.Set Address
        , _nextSlot     :: Maybe Slot
        , _endpoints    :: Set.Set String -- TODO: better type - Map String EndpointSchema?
        }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (Aeson.FromJSON, Aeson.ToJSON)

instance Semigroup Hooks where
    l <> r = Hooks
                { _transactions = _transactions l <> _transactions r
                , _addresses    = _addresses l <> _addresses r
                , _nextSlot     = fmap getMin $ getOption $ Option (Min <$> _nextSlot l) <> Option (Min <$> _nextSlot r)
                , _endpoints    = _endpoints l <> _endpoints r
                }

instance Monoid Hooks where
    mappend = (<>)
    mempty = Hooks mempty mempty Nothing mempty

data BalancedHooks =
    BalancedHooks
        { _bTransactions :: [UnbalancedTx] -- TODO: Track the txns with some kind of ID
        , _bAddresses    :: Map.Map Address Balanced
        , _bNextSlot     :: Map.Map Slot Balanced
        -- ^ List of slots that the contract is waiting for
        , _bEndpoints    :: Map.Map String Balanced
        } 
        deriving stock (Eq, Show, Generic)
        deriving anyclass (Aeson.FromJSON, Aeson.ToJSON)

openKeys :: Map.Map k Balanced -> Set.Set k
openKeys = Map.keysSet . Map.filter isOpen

instance Semigroup BalancedHooks where
    l <> r =
        BalancedHooks
            { _bTransactions = _bTransactions l <> _bTransactions r
            , _bAddresses    = Map.unionWith (<>) (_bAddresses l) (_bAddresses r)
            , _bNextSlot     = Map.unionWith (<>) (_bNextSlot l) (_bNextSlot r)
            , _bEndpoints    = Map.unionWith (<>) (_bEndpoints l) (_bEndpoints r)
            }

instance Monoid BalancedHooks where
    mempty = BalancedHooks mempty Map.empty Map.empty Map.empty

fromBalanced :: BalancedHooks -> Hooks
fromBalanced bs =
    Hooks
        { _transactions = _bTransactions bs
        , _addresses = openKeys (_bAddresses bs)
        , _nextSlot  = Set.lookupMin (openKeys (_bNextSlot bs))
        , _endpoints = openKeys (_bEndpoints bs)
        }

tx :: UnbalancedTx -> BalancedHooks
tx t = mempty { _bTransactions = [t] }

addr :: Address -> BalancedHooks
addr a = mempty { _bAddresses = Map.singleton a open }

closeAddr :: Address -> BalancedHooks
closeAddr a = mempty { _bAddresses = Map.singleton a close }

slot :: Slot -> BalancedHooks
slot s = mempty { _bNextSlot = Map.singleton s open }

closeSlot :: Slot -> BalancedHooks
closeSlot s = mempty { _bNextSlot = Map.singleton s close }

endpointName :: String -> BalancedHooks
endpointName e = mempty { _bEndpoints = Map.singleton e open }

closeEndpoint :: String -> BalancedHooks
closeEndpoint e = mempty { _bEndpoints = Map.singleton e close }
