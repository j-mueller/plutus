{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
module Language.Plutus.Contract.Contract(
      ContractPrompt
    , loopM
    , foldMaybe
    , selectEither
    , both
    -- * Feed events to the contract and look at the outputs
    , runContract
    ) where

import           Control.Applicative              (Alternative (empty))
import           Control.Monad.Prompt             (MonadPrompt (..), PromptT, runPromptTM)
import           Control.Monad.State
import           Control.Monad.Writer

import           Language.Plutus.Contract.Class
import           Language.Plutus.Contract.Event   as Event
import           Language.Plutus.Contract.Hooks   as Hooks
import           Language.Plutus.Contract.Request

-- | An instance of 'PlutusContract Event (Hook ())'
--   that uses the 'PromptT' type
newtype ContractPrompt f a = ContractPrompt { unPlutusContract :: PromptT (Hook ()) Event f a }
    deriving (Functor, Applicative, Monad, Alternative, MonadPrompt (Hook ()) Event)

-- | Apply the events in the state. If there were enough events to satisfy
--   all the requests, then 'pure a' is returned and nothing is written.
--   If there aren't enough, then 'empty' is returned and the missing hooks
--   are written.
runContract
    :: forall f m a.
       ( Alternative f
       , MonadState [Event] m
       , MonadWriter Hooks m)
    => ContractPrompt f a
    -> m (f a)
runContract = flip runPromptTM go . unPlutusContract where
    go hks = do
        evts <- get
        let go' = \case
                    [] -> tell (hooks hks) >> pure empty
                    e:es -> case match hks e of
                        Nothing -> go' es
                        Just e' -> put es >> pure (pure e')
        go' evts
