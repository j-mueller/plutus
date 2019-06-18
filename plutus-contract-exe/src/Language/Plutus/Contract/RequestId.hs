{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
module Language.Plutus.Contract.RequestId where

import           Control.Monad.State (MonadState (..), gets)
import qualified Data.Aeson          as Aeson
import           GHC.Generics        (Generic)

newtype RequestId = RequestId Int
    deriving stock (Eq, Ord, Show, Generic)
    deriving newtype (Aeson.FromJSON, Aeson.ToJSON, Aeson.ToJSONKey, Aeson.FromJSONKey, Enum, Num)

freshId :: MonadState RequestId m => m RequestId
freshId = do
    i <- gets succ
    put i
    pure i

