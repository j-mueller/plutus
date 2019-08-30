{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DerivingVia          #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE OverloadedLabels     #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
module Language.Plutus.Contract.Request where

import qualified Data.Aeson                         as Aeson
import           Data.Row

import           Language.Plutus.Contract.Events    (Event (..), First, Hooks (..), Second)
import qualified Language.Plutus.Contract.Events    as Events
import           Language.Plutus.Contract.Resumable

-- | @Contract i o a@ is a contract that expects input events of type @i@ and produces
--   requests (describing the acceptable input) of type @o@. The two type parameters
--   are 'Data.Row.Row' rows
--
type Contract s a = Resumable (Step (Maybe (Event s)) (Hooks s)) a

type ContractRow s =
  ( Forall (Second s) Monoid
  , Forall (Second s) Semigroup
  , AllUniqueLabels (First s)
  , AllUniqueLabels (Second s)
  )

requestMaybe
  :: forall l req resp s a.
     ( KnownSymbol l
     , HasType l resp (First s)
     , HasType l req (Second s)
     , ContractRow s
     )
    => req
    -> (resp -> Maybe a)
    -> Contract s a
requestMaybe out check = do
  rsp <- request @l @req @resp @s out
  case check rsp of
    Nothing -> requestMaybe @l @req @resp @s out check
    Just a  -> pure a

request
  :: forall l req resp s.
    ( KnownSymbol l
    , HasType l resp (First s)
    , HasType l req (Second s)
    , ContractRow s
    )
    => req
    -> Contract s resp
request out = CStep (Step go) where
  upd = Left $ Events.initialise @s @l out
  go Nothing = upd
  go (Just (Event rho)) = case trial rho (Label @l) of
    Left resp -> Right resp
    _         -> upd

select :: forall s a. Contract s a -> Contract s a -> Contract s a
select = CAlt

cJSONCheckpoint :: forall s a. (Aeson.FromJSON a, Aeson.ToJSON a) => Contract s a -> Contract s a
cJSONCheckpoint = CJSONCheckpoint
