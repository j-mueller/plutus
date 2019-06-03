{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
module Language.Plutus.Contract.Step where

import qualified Data.Aeson                           as Aeson
import           Data.Map                             (Map)
import qualified Data.Map                             as Map
import           Data.Semigroup                       (Min (..), Option (..))
import qualified Data.Set                             as Set
import           GHC.Generics                         (Generic)

import           Language.Plutus.Contract.Transaction (UnbalancedTx)
import           Ledger.AddressMap                    (AddressMap)
import qualified Ledger.AddressMap                    as AM
import           Ledger.Slot                          (Slot)
import           Ledger.Tx                            (Address, Tx)

data Step =
    Step
        { stepTransactions :: [UnbalancedTx]
        , stepAddresses    :: Set.Set Address
        , stepNextSlot     :: Maybe Slot
        , stepEndpoints    :: Set.Set String -- TODO: better type - Map String EndpointSchema?
        }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (Aeson.FromJSON, Aeson.ToJSON)

instance Semigroup Step where
    l <> r = Step
                { stepTransactions = stepTransactions l <> stepTransactions r
                , stepAddresses    = stepAddresses l <> stepAddresses r
                , stepNextSlot     = fmap getMin $ getOption $ Option (Min <$> stepNextSlot l) <> Option (Min <$> stepNextSlot r)
                , stepEndpoints    = stepEndpoints l <> stepEndpoints r
                }

instance Monoid Step where
    mappend = (<>)
    mempty = Step mempty mempty Nothing mempty

tx :: UnbalancedTx -> Step
tx t = mempty { stepTransactions = [t] }

addr :: Address -> Step
addr a = mempty { stepAddresses = Set.singleton a }

slot :: Slot -> Step
slot s = mempty { stepNextSlot = Just s }

endpointName :: String -> Step
endpointName e = mempty { stepEndpoints = Set.singleton e }
  