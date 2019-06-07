{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
module Language.Plutus.Contract.Step where

import qualified Data.Aeson                           as Aeson
import qualified Data.Map                             as Map
import           Data.Semigroup                       (Min (..), Option (..))
import qualified Data.Set                             as Set
import           GHC.Generics                         (Generic)

import           Language.Plutus.Contract.Transaction (UnbalancedTx)
import           Ledger.Slot                          (Slot)
import           Ledger.Tx                            (Address)

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

class Semigroup m => InverseSemigroup m where
    inv :: m -> m

-- 'Balanced' keeps track of how many things (parens, endpoint calls, etc.) are required
-- on either side.
--
-- From https://www.youtube.com/watch?v=HGi5AxmQUwU
data Balanced = Balanced !Int !Int
    deriving stock (Eq, Show, Generic)
    deriving anyclass (Aeson.FromJSON, Aeson.ToJSON)

instance Semigroup Balanced where
    Balanced a b <> Balanced c d
        | b <= c = Balanced (a + c - b) d
        | otherwise = Balanced a (d + b - c)

instance InverseSemigroup Balanced where
    inv (Balanced a b) = Balanced b a

open :: Balanced
open = Balanced 0 1

close :: Balanced
close = Balanced 1 0

instance Monoid Balanced where
    mempty = Balanced 0 0

isBalanced :: Balanced -> Bool
isBalanced = (==) mempty

isOpen :: Balanced -> Bool
isOpen (Balanced _ o) = o > 0

data BalancedStep =
    BalancedStep
        { bStepTransactions :: [UnbalancedTx]
        , bStepAddresses    :: Map.Map Address Balanced
        , bStepNextSlot     :: Maybe Slot
        , bStepEndpoints    :: Map.Map String Balanced
        } deriving Show

openKeys :: Map.Map k Balanced -> Set.Set k
openKeys = Map.keysSet . Map.filter isOpen

instance Semigroup BalancedStep where
    l <> r = 
        BalancedStep
            { bStepTransactions = bStepTransactions l <> bStepTransactions r
            , bStepAddresses    = Map.unionWith (<>) (bStepAddresses l) (bStepAddresses r)
            , bStepNextSlot     = fmap getMin $ getOption $ Option (Min <$> bStepNextSlot l) <> Option (Min <$> bStepNextSlot r)
            , bStepEndpoints    = Map.unionWith (<>) (bStepEndpoints l) (bStepEndpoints r)
            }

instance Monoid BalancedStep where
    mempty = BalancedStep mempty Map.empty Nothing Map.empty

step :: BalancedStep -> Step
step bs = 
    Step
        { stepTransactions = bStepTransactions bs
        , stepAddresses = openKeys (bStepAddresses bs)
        , stepNextSlot  = bStepNextSlot bs
        , stepEndpoints = openKeys (bStepEndpoints bs)
        }

tx :: UnbalancedTx -> BalancedStep
tx t = mempty { bStepTransactions = [t] }

addr :: Address -> BalancedStep
addr a = mempty { bStepAddresses = Map.singleton a open }

closeAddr :: Address -> BalancedStep
closeAddr a = mempty { bStepAddresses = Map.singleton a close }

slot :: Slot -> BalancedStep
slot s = mempty { bStepNextSlot = Just s }

endpointName :: String -> BalancedStep
endpointName e = mempty { bStepEndpoints = Map.singleton e open }

closeEndpoint :: String -> BalancedStep
closeEndpoint e = mempty { bStepEndpoints = Map.singleton e close }
