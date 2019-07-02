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
    (TxHook _, TxSubmission) -> Just r
    _ -> Nothing

{- note  [Hooks and Events]

The three types 'Hook', 'Hooks' and 'Event' are closely related.

* `Hook a` values are produced by the smart contract. They describe events that 
  the contract is waiting for. For example, `AddrHook :: Address -> Hook a` 
  means "I want to know the next transaction that affects this address". The 
  type parameter `a` is currently always the unit type, so it is effectively 
  unused. This parameter will hold information about the type (schema) of 
  endpoints that the contract exposes.

* `Hooks` is a set of all hooks that the contract exposes in its current state.
  The contract may have multiple branches running in parallel, and when a branch
  gets blocked on a `Hook ()`, that hook is included in the `Hooks` value that
  is returned to the client.

* `Event` values are produced by the app platform. Each constructor of `Event` 
  corresponds to a constructor of `Hook a`. For example, `LedgerUpdate :: 
  Address -> Tx -> Event` is the counterpart of `AddrHook`.

To the outside world, the contract is essentially a state machine `State -> Event -> (Hooks, State)` where `State` is an opaque blob that the caller
needs to store after each call. The `Event` should correspond to one of the `Hooks` of the previous call. 

TO DOs

1. Use the type parameter of `Hooks` to encode the type of endpoints that a 
   contract may expose. Once we can do that, we can generalise the notion of
    endpoints to mean any kind of capability that the contract may need (submit 
    transactions, sign things, make HTTP calls, etc), not just user-facing 
    endpoints. Then we can get rid of all other constructors in `Hook` and 
    `Event`.

2. Directly match `Event`s to `Hook`s, using request IDs. This will make 
   the contract code slightly more efficient since we won't have to try the same
   event in multiple branches anymore, and it would be especially useful for the
   `SubmitTransaction` hook because we currently can't refer to the transaction 
   after submitting it.

-}
