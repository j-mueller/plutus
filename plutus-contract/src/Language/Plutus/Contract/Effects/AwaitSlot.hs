{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE TypeOperators    #-}
module Language.Plutus.Contract.Effects.AwaitSlot where

import           Data.Row
import           Data.Semigroup
import           Prelude                          hiding (until)

import           Language.Plutus.Contract.Events  (Event (..), Hooks (..))
import           Language.Plutus.Contract.Request as Req
import           Language.Plutus.Contract.Util    (foldMaybe, selectEither)

import           Ledger.Slot                      (Slot)

type SlotPrompt i o =
  ( HasType "slot" Slot i
  , HasType "slot" (Maybe (Min Slot)) o
  , ContractRow i o
  )
type SlotIn = "slot" .== Slot
type SlotOut = "slot" .== Maybe (Min Slot)

-- | A contract that waits until the slot is reached, then returns the
--   current slot.
awaitSlot
    :: forall i o.
       (SlotPrompt i o)
    => Slot
    -> Contract i o Slot
awaitSlot sl =
  let s = Just $ Min sl
      check :: Slot -> Maybe Slot
      check sl' = if sl' >= sl then Just sl' else Nothing
  in
  requestMaybe @"slot" s check

event
    :: forall i.
    ( HasType "slot" Slot i
    , AllUniqueLabels i)
    => Slot
    -> Event i
event = Event . IsJust #slot

nextSlot
    :: forall i.
    ( HasType "slot" (Maybe (Min Slot)) i)
    => Hooks i
    -> Maybe Slot
nextSlot (Hooks r) = fmap getMin (r .! #slot)


-- | Run a contract until the given slot has been reached.
until
  :: forall i o a.
     (SlotPrompt i o)
  => Contract i o a
  -> Slot
  -> Contract i o (Maybe a)
until c sl = 
  fmap (either (const Nothing) Just) (selectEither (awaitSlot sl) c)

-- | Run a contract when the given slot has been reached.
when
  :: forall i o a.
     (SlotPrompt i o)
  => Slot
  -> Contract i o a
  -> Contract i o a
when s c = awaitSlot s >> c

-- | Run a contract until the given slot has been reached.
--   @timeout = flip until@
timeout
  :: forall i o a.
     (SlotPrompt i o)
  => Slot
  -> Contract i o a
  -> Contract i o (Maybe a)
timeout = flip until

-- | Wait until the first slot is reached, then run the contract until
--   the second slot is reached.
between
  :: forall i o a.
     (SlotPrompt i o)
  => Slot
  -> Slot
  -> Contract i o a
  -> Contract i o (Maybe a)
between a b = timeout b . when a

-- The constraints are a bit annoying :(

-- | Repeatedly run a contract until the slot is reached, then
--   return the last result.
collectUntil
  :: forall i o a b.
     (SlotPrompt i o)
  => (a -> b -> b)
  -> b
  -> Contract i o a
  -> Slot
  -> Contract i o b
collectUntil f b con s = foldMaybe f b (until con s)
