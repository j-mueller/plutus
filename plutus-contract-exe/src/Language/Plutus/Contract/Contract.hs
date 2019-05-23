{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE TypeFamilies      #-}
module Language.Plutus.Contract.Contract(
      Contract
    , emit
    , waiting
    , applyInput
    , applyInputs
    , drain
    , outputs
    , await
    , result
    , loopM
    ) where

import           Control.Applicative (liftA2)
import           Control.Monad       ((>=>))
import           Data.Bifunctor      (first)
import           Data.List           (foldl')

data Contract i o a =
    Waiting (i -> Contract i o a)
    | Emit o (Contract i o a) -- produce a 't' value
    | Pure a
    deriving (Functor)

-- The applicative instance parallelises the 'Waiting' operations
instance Applicative (Contract i o) where
    pure = Pure
    cf <*> ca = case cf of
        Emit t c -> Emit t (c <*> ca)
        Pure f -> fmap f ca
        Waiting f ->
            case ca of
                Emit t c -> Emit t (Waiting f <*> c)
                Pure a' -> Waiting $ \i' -> fmap (\f' -> f' a') (f i')
                Waiting f' ->
                    Waiting $ \i' -> f i' <*> f' i'

-- The monad instance sequentialises the 'Waiting' operations
instance Monad (Contract i o) where
    c >>= f = case c of
        Waiting f' -> Waiting (f' >=> f)
        Pure a     -> f a
        Emit t c'  -> Emit t (c' >>= f)

instance Semigroup a => Semigroup (Contract i o a) where
    (<>) = liftA2 (<>)

emit :: o -> Contract i o ()
emit t = Emit t (pure ())

waiting :: Contract i o i
waiting = Waiting pure

-- https://hackage.haskell.org/package/extra-1.6.15/docs/src/Control.Monad.Extra.html#loopM

-- | A monadic version of 'loop', where the predicate returns 'Left' as a seed for the next loop
--   or 'Right' to abort the loop.
loopM :: Monad m => (a -> m (Either a b)) -> a -> m b
loopM act x = do
    res <- act x
    case res of
        Left x' -> loopM act x'
        Right v -> return v

-- | Apply an input to a contract, collecting as much output data
--   't' as posible until the contract is blocked on inputs
applyInput
    :: Monoid o
    => i
    -> Contract i o a
    -> (o, Contract i o a)
applyInput ip = \case
    Waiting f -> drain (f ip)
    Pure a -> (mempty, Pure a)
    Emit t c -> first (t <>) (applyInput ip c)

applyInputs
    :: Monoid o
    => Contract i o a
    -> [i]
    -> ([o], Contract i o a)
applyInputs c = foldl' go (first return (drain c)) where
    go (ts, c') i = first (:ts) (applyInput i c')

drain :: Monoid o => Contract i o a -> (o, Contract i o a)
drain = \case
    Waiting f -> (mempty, Waiting f)
    Pure a -> (mempty, Pure a)
    Emit t c -> first (t <>) (drain c)

outputs :: Monoid o => Contract i o a -> o
outputs = fst . drain

result :: Monoid o => Contract i o a -> Maybe a
result = (\case { Pure b -> Just b; _ -> Nothing }) . snd . drain

await :: o -> (i -> Maybe a) -> Contract i o a
await a f = do
    emit a
    i <- waiting
    case f i of
        Nothing -> await a f
        Just i' -> pure i'
