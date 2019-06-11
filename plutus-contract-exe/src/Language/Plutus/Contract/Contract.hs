{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
module Language.Plutus.Contract.Contract(
      Contract(..)
    , emit
    , offer
    , waiting
    , applyInputs
    , await
    , loopM
    , foldMaybe
    , selectEither
    , select
    , both
    -- * Feed events to the contract and look at the outputs
    , drain
    , outputs
    , result
    ) where

import           Control.Applicative            (liftA2)
import           Control.Monad                  ((>=>))
import           Data.Bifunctor                 (first)
import           Data.Functor.Alt

import           Language.Plutus.Contract.Class

data Contract i o a =
    Waiting (i -> Contract i o a)
    | Emit o (Contract i o a) -- produce a 't' value
    | Pure a
    deriving (Functor)

instance Alt (Contract i o) where
    (<!>) = select

-- The applicative instance parallelises the 'Waiting' operations
instance Applicative (Contract i o) where
    pure = Pure
    Emit t c  <*> ca       = Emit t (c <*> ca)
    cf        <*> Emit t c = Emit t (cf <*> c)
    Pure f    <*> ca       = f <$> ca
    Waiting f <*> ca       = Waiting $ \i -> f i <*> offer i ca

instance MonadContract i o (Contract i o) where
    emit t = Emit t (pure ())
    waiting = Waiting pure

    select (Emit e c) r = Emit e (select c r)
    select l (Emit e c) = Emit e (select l c)
    select (Pure a)   _ = Pure a
    select _   (Pure a) = Pure a
    select (Waiting f) (Waiting f') =
        Waiting $ \i -> select (f i) (f' i)

    offer i (Waiting f) = f i
    offer i (Emit t c)  = Emit t (offer i c)
    offer _ c           = c

-- The monad instance sequentialises the 'Waiting' operations
instance Monad (Contract i o) where
    c >>= f = case c of
        Waiting f' -> Waiting (f' >=> f)
        Pure a     -> f a
        Emit t c'  -> Emit t (c' >>= f)

instance Semigroup a => Semigroup (Contract i o a) where
    (<>) = liftA2 (<>)

drain :: Monoid o => Contract i o a -> (o, Contract i o a)
drain = \case
    Waiting f -> (mempty, Waiting f)
    Pure a -> (mempty, Pure a)
    Emit t c -> first (t <>) (drain c)

outputs :: Monoid o => Contract i o a -> o
outputs = fst . drain

result :: Monoid o => Contract i o a -> Either o a
result = \case
    Pure a -> Right a
    c'     -> Left (fst (drain c'))
