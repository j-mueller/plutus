{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
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
    , initial
    , InstanceState(..)
    ) where

import           Control.Applicative                (Alternative)
import           Control.Lens                       hiding (both)
import           Control.Monad.Prompt               (MonadPrompt (..), PromptT, hoistP, runPromptTM)
import           Control.Monad.Reader
import           Control.Monad.RWS.Lazy
import           Control.Monad.State
import           Data.Bifunctor
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

data InstanceState =
    InstanceState
        { _events   :: Map.Map RequestId Event
        , _lastTxId :: RequestId
        }

makeLenses ''InstanceState

initial :: InstanceState
initial = InstanceState Map.empty 0

applyEvents
    :: forall m n a.
       ( MonadState InstanceState m
       , MonadReader InstanceState m
       , Functor (Zoomed n (Hooks, RequestId))
       , Zoom n m RequestId InstanceState )
    => ContractPrompt (Either Hooks) a
    -> m (Validation Hooks a)
applyEvents = flip runPromptTM go . hoistP eitherToValidation validationToEither . unPlutusContract where
    go hks = do
        (hks', i) <- zoom lastTxId (hooks hks)
        evts <- view events
        case Map.lookup i evts >>= match hks of
            Nothing -> pure (eitherToValidation $ Left hks')
            Just e' -> pure (eitherToValidation $ Right e')

runContract
    :: forall a. ContractPrompt (Either Hooks) a
    -> InstanceState
    -> (Either Hooks a, RequestId)
runContract con s =
    first validationToEither
    $ second (view lastTxId)
    $ runReader (runStateT (applyEvents con) s) s

runContract' :: forall a. ContractPrompt (Either Hooks) a -> Map.Map RequestId Event -> (Maybe a, Hooks)
runContract' con es =
    let (r, _) = runContract con (InstanceState es 0)
    in either (\h -> (Nothing, h)) (\a -> (Just a, mempty)) r
