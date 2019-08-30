{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
module Language.Plutus.Contract.Effects.AwaitSlot where

import           Data.Row
import           Data.Semigroup                   (Min (..))
import           Prelude                          hiding (until)

import           Language.Plutus.Contract.Request as Req
import           Language.Plutus.Contract.Schema  (Event (..), First, Hooks (..), Second)
import           Language.Plutus.Contract.Util    (foldMaybe, selectEither)

import           Ledger.Slot                      (Slot)

type HasAwaitSlot s =
  ( HasType "slot" Slot (First s)
  , HasType "slot" (Maybe (Min Slot)) (Second s)
  , ContractRow s
  )

type AwaitSlot = "slot" .== (Slot, Maybe (Min Slot))

-- | A contract that waits until the slot is reached, then returns the
--   current slot.
awaitSlot
    :: forall s.
       (HasAwaitSlot s)
    => Slot
    -> Contract s Slot
awaitSlot sl =
  let s = Just $ Min sl
      check :: Slot -> Maybe Slot
      check sl' = if sl' >= sl then Just sl' else Nothing
  in
  requestMaybe @"slot" @_ @_ @s s check

event
    :: forall s.
    ( HasType "slot" Slot (First s)
    , AllUniqueLabels (First s))
    => Slot
    -> Event s
event = Event . IsJust #slot

nextSlot
    :: forall s.
    ( HasType "slot" (Maybe (Min Slot)) (Second s))
    => Hooks s
    -> Maybe Slot
nextSlot (Hooks r) = fmap getMin (r .! #slot)


-- | Run a contract until the given slot has been reached.
until
  :: forall s a.
     (HasAwaitSlot s)
  => Contract s a
  -> Slot
  -> Contract s (Maybe a)
until c sl =
  fmap (either (const Nothing) Just) (selectEither (awaitSlot @s sl) c)

-- | Run a contract when the given slot has been reached.
when
  :: forall s a.
     (HasAwaitSlot s)
  => Slot
  -> Contract s a
  -> Contract s a
when s c = awaitSlot @s s >> c

-- | Run a contract until the given slot has been reached.
--   @timeout = flip until@
timeout
  :: forall s a.
     (HasAwaitSlot s)
  => Slot
  -> Contract s a
  -> Contract s (Maybe a)
timeout = flip (until @s)

-- | Wait until the first slot is reached, then run the contract until
--   the second slot is reached.
between
  :: forall s a.
     (HasAwaitSlot s)
  => Slot
  -> Slot
  -> Contract s a
  -> Contract s (Maybe a)
between a b = timeout @s b . when @s a

-- The constraints are a bit annoying :(

-- | Repeatedly run a contract until the slot is reached, then
--   return the last result.
collectUntil
  :: forall s a b.
     (HasAwaitSlot s)
  => (a -> b -> b)
  -> b
  -> Contract s a
  -> Slot
  -> Contract s b
collectUntil f b con s = foldMaybe f b (until @s con s)
