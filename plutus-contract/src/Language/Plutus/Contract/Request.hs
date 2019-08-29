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
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
module Language.Plutus.Contract.Request where

import qualified Data.Aeson                         as Aeson
import           Data.Row
import           Language.Plutus.Contract.Events    (Event (..), Hooks (..))
import qualified Language.Plutus.Contract.Events    as Events
import           Language.Plutus.Contract.Resumable

-- | @Contract i o a@ is a contract that expects input events of type @i@ and produces
--   requests (describing the acceptable input) of type @o@. The two type parameters
--   are 'Data.Row.Row' rows
--
type Contract i o a = Resumable (Step (Maybe (Event i)) (Hooks o)) a

type ContractRow i o =
  ( Forall o Monoid
  , Forall o Semigroup
  , AllUniqueLabels o
  , AllUniqueLabels i)

requestMaybe
  :: forall s req resp i o a.
     ( KnownSymbol s
     , HasType s resp i
     , HasType s req o
     , ContractRow i o
     )
    => req
    -> (resp -> Maybe a)
    -> Contract i o a
requestMaybe out check = do
  rsp <- request @s out
  case check rsp of
    Nothing -> requestMaybe @s @req @resp @i @o out check
    Just a  -> pure a

request
  :: forall s req resp i o.
    ( KnownSymbol s
    , HasType s resp i
    , HasType s req o
    , ContractRow i o
    )
    => req
    -> Contract i o resp
request out = CStep (Step go) where
  upd = Left $ Events.initialise @o @s out
  go Nothing = upd
  go (Just (Event rho)) = case trial rho (Label @s) of
    Left resp -> Right resp
    _         -> upd

select :: forall i o a. Contract i o a -> Contract i o a -> Contract i o a
select = CAlt

cJSONCheckpoint :: forall i o a. (Aeson.FromJSON a, Aeson.ToJSON a) => Contract i o a -> Contract i o a
cJSONCheckpoint = CJSONCheckpoint
