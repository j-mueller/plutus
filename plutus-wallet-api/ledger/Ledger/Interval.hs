{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE MonoLocalBinds       #-}
-- | A type for intervals and associated functions.
module Ledger.Interval(
      Interval(..)
    , interval
    , from
    , to
    , always
    ) where

import qualified Ledger.Interval.TH                       as TH
import           Ledger.Interval.TH (Interval(..))

-- | An 'Interval' that covers every slot.
always :: Interval a
always = $$(TH.always)

-- | @from a@ is an 'Interval' that includes all values that are
--  greater than or equal to @a@.
from :: a -> Interval a
from = $$(TH.from)

-- | @to a@ is an 'Interval' that includes all values that are
--  smaller than @a@.
to :: a -> Interval a
to = $$(TH.to)

-- | @interval a b@ includes all values that are greater than or equal
--   to @a@ and smaller than @b@. Therefore it includes @a@ but not it
--   does not include @b@.
interval :: a -> a -> Interval a
interval = $$(TH.interval)
