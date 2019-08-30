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

import           Language.Plutus.Contract.IOTS
import           Language.Plutus.Contract.Request as Req
import           Language.Plutus.Contract.Schema  (Event (..), First, Hooks (..), Second)

newtype EndpointDescription = EndpointDescription { getEndpointDescription :: String }
    deriving stock (Eq, Ord, Generic)
    deriving newtype (ToJSON, FromJSON)
    deriving anyclass (IotsType)

type EndpointPrompt l a s =
  ( HasType l a (First s)
  , HasType l (Set EndpointDescription) (Second s)
  , KnownSymbol l
  , ContractRow s
  )

type Endpoint l a = l .== (a, Set EndpointDescription)

-- | Expose an endpoint, return the data that was entered
endpoint
  :: forall l a s.
     ( EndpointPrompt l a s )
  => Contract s a
endpoint = request @l @_ @_ @s s where
  s = Set.singleton $ EndpointDescription $ symbolVal (Proxy @l)

event
  :: forall (l :: Symbol) a s. (KnownSymbol l, HasType l a (First s), AllUniqueLabels (First s))
  => a
  -> Event s
event = Event . IsJust (Label @l)

isActive
  :: forall (l :: Symbol) s. (KnownSymbol l, HasType l (Set EndpointDescription) (Second s))
  => Hooks s
  -> Bool
isActive (Hooks r) = not $ Set.null $ r .! Label @l
