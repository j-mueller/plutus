{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs        #-}
{-# LANGUAGE TypeFamilies #-}
module Language.Plutus.Contract.NewCon where

import           Control.Lens                         hiding (both)
import           Control.Monad                        ((>=>))
import           Data.Aeson                           (FromJSON)
import qualified Data.Aeson                           as Aeson
import Data.Functor.Apply (Apply ((<.>)))
import           Data.Maybe                           (fromMaybe)

import           Language.Plutus.Contract.Class       (MonadContract (..))
import           Language.Plutus.Contract.Contract    as Contract
import           Language.Plutus.Contract.Event       as Event hiding (endpoint)
import           Language.Plutus.Contract.Hooks       as Hooks
import           Language.Plutus.Contract.Transaction as Transaction

import           Ledger.AddressMap                    (AddressMap)
import qualified Ledger.AddressMap                    as AM
import           Ledger.Slot                          (Slot)
import           Ledger.Tx                            (Address, Tx)
import           Ledger.Value                         (Value)
import qualified Ledger.Value                         as V

import           Prelude                              hiding (until)

data TxStatus = Submit | Reject

class Req t where
    type Request t :: *
    type Response t :: *

type family EndpointProd a b :: *

data Blocked t f a where
    WaitingForSlot :: Slot -> (Slot -> f a) -> Blocked t f a
    WaitingForTxConfirm :: UnbalancedTx -> (TxStatus -> f a) -> Blocked t f a
    WaitingForEndpoint :: EndpointInfo t -> (EndpointReq t -> f a) -> Blocked t f a
    WaitingPair :: Blocked r f a -> Blocked s f a -> Blocked (EndpointProd r s) f a

-- capability
-- HasCapability d m => Running (Request d) (Response d -> a)

instance Functor f => Functor (Blocked t f) where
    fmap f = \case
        WaitingForSlot sl f' -> WaitingForSlot sl (fmap (fmap f) f')
        WaitingForTxConfirm tx f' -> WaitingForTxConfirm tx (fmap (fmap f) f')
        WaitingForEndpoint ep f' -> WaitingForEndpoint ep (fmap (fmap f) f')
        WaitingPair l r -> WaitingPair (fmap f l) (fmap f r)

-- newtype NewCon t f a = Contract (Either (Blocked t f a) a)

