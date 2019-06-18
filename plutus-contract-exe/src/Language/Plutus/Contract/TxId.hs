{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
-- | Identifies for unbalanced transactions
module Language.Plutus.Contract.TxId where

import           Control.Monad.State (MonadState (..), gets)
import qualified Data.Aeson          as Aeson
import           GHC.Generics        (Generic)

newtype UnbalancedTxId = UnbalancedTxId Int
    deriving stock (Eq, Ord, Show, Generic)
    deriving newtype (Aeson.FromJSON, Aeson.ToJSON, Aeson.ToJSONKey, Aeson.FromJSONKey, Enum, Num)

freshId :: MonadState UnbalancedTxId m => m UnbalancedTxId
freshId = do
    i <- gets succ
    put i
    pure i
