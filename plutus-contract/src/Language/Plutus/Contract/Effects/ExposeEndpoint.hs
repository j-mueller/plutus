{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
module Language.Plutus.Contract.Effects.ExposeEndpoint where

import           Data.Aeson                       (FromJSON, ToJSON)
import           Data.Proxy
import           Data.Row
import           Data.Set                         (Set)
import qualified Data.Set                         as Set
import           GHC.Generics                     (Generic)
import           GHC.TypeLits                     (Symbol, symbolVal)

import           Language.Plutus.Contract.Events  (Event (..), Hooks (..))
import           Language.Plutus.Contract.IOTS
import           Language.Plutus.Contract.Request as Req

newtype EndpointDescription = EndpointDescription { getEndpointDescription :: String }
    deriving stock (Eq, Ord, Generic)
    deriving newtype (ToJSON, FromJSON)
    deriving anyclass (IotsType)

type EndpointPrompt s a i o =
  ( HasType s a i
  , HasType s (Set EndpointDescription) o
  , KnownSymbol s
  , ContractRow i o
  )

type EndpointIn s a = s .== a
type EndpointOut s = s .== Set EndpointDescription

-- | Expose an endpoint, return the data that was entered
endpoint 
  :: forall s a i o. 
     ( EndpointPrompt s a i o )
  => Contract i o a
endpoint = request @s s where
  s = Set.singleton $ EndpointDescription $ symbolVal (Proxy @s)

event
  :: forall (s :: Symbol) i a. (KnownSymbol s, HasType s a i, AllUniqueLabels i)
  => a
  -> Event i
event = Event . IsJust (Label @s)

isActive
  :: forall (s :: Symbol) o. (KnownSymbol s, HasType s (Set EndpointDescription) o)
  => Hooks o
  -> Bool
isActive (Hooks r) = not $ Set.null $ r .! Label @s
