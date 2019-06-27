{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
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
    , runContract'
    ) where

import           Control.Applicative                (Alternative)
import           Control.Monad.Prompt               (MonadPrompt (..), PromptT, hoistP, runPromptTM)
import           Control.Monad.RWS.Lazy
import           Data.Either.Validation
import qualified Data.Map                           as Map

import           Language.Plutus.Contract.Class
import           Language.Plutus.Contract.Event     as Event
import           Language.Plutus.Contract.Hooks     as Hooks
import           Language.Plutus.Contract.Request
import           Language.Plutus.Contract.RequestId

-- | An instance of 'PlutusContract Event (Hook ())'
--   that uses the 'PromptT' type
newtype ContractPrompt f a = ContractPrompt { unPlutusContract :: PromptT (Hook ()) Event f a }
    deriving (Functor, Applicative, Monad, Alternative, MonadPrompt (Hook ()) Event)

applyEvents
    :: forall m a.
       ( MonadReader (Map.Map RequestId Event) m
       , MonadState RequestId m )
    => ContractPrompt (Either Hooks) a
    -> m (Validation Hooks a)
applyEvents = flip runPromptTM go . hoistP eitherToValidation validationToEither . unPlutusContract where
    go :: Hook () -> m (Validation Hooks Event)
    go hks = do
        (hks', i) <- hooks hks -- FIXME needs to be stable
        evts <- ask
        case Map.lookup i evts >>= match hks of
            Nothing -> pure (eitherToValidation $ Left hks')
            Just e' -> pure (eitherToValidation $ Right e')

runContract
    :: forall m a.
        ( MonadReader (Map.Map RequestId Event) m
        , MonadState RequestId m)
    => ContractPrompt (Either Hooks) a
    -> m (Either Hooks a)
runContract con = validationToEither <$> applyEvents con

runContract' 
    :: forall m a. 
        ( MonadReader (Map.Map RequestId Event) m
        , MonadState RequestId m)
    => ContractPrompt (Either Hooks) a
    -> m (Maybe a, Hooks)
runContract' con =
    either (\h -> (Nothing, h)) (\a -> (Just a, mempty)) <$> runContract con
