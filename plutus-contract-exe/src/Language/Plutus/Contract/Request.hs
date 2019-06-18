-- | Matching of Request-Response pairs for a contract
module Language.Plutus.Contract.Request where

import           Language.Plutus.Contract.Event
import           Language.Plutus.Contract.Hooks

match :: Hook a -> Event -> Maybe Event
match l r = case (l, r) of
    (AddrHook addr, LedgerUpdate addr' _)
        | addr == addr' -> Just r
    (SlotHook sl, SlotChange sl')
        | sl <= sl' -> Just r
    (EndpointHook n _, Endpoint n' _)
        | n == n' -> Just r
    _ -> Nothing

{- note  [Hooks and Events]

The three types 'Hook', 'Hooks' and 'Event' are closely related.

FIXME: Document them and write about future plans.

-}
