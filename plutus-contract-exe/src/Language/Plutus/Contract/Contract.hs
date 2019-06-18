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

import           Control.Applicative              (Alternative)
import           Control.Lens                     hiding (both)
import           Control.Monad.Prompt             (MonadPrompt (..), PromptT, runPromptTM)
import           Control.Monad.RWS.Lazy

import           Language.Plutus.Contract.Class
import           Language.Plutus.Contract.Event   as Event
import           Language.Plutus.Contract.Hooks   as Hooks
import           Language.Plutus.Contract.Request
import           Language.Plutus.Contract.TxId

-- | An instance of 'PlutusContract Event (Hook ())' 
--   that uses the 'PromptT' type
newtype ContractPrompt f a = ContractPrompt { unPlutusContract :: PromptT (Hook ()) Event f a }
    deriving (Functor, Applicative, Monad, Alternative, MonadPrompt (Hook ()) Event)

data InstanceState =
    InstanceState
        { _events   :: [Event]
        , _lastTxId :: UnbalancedTxId
        }

makeLenses ''InstanceState

initial :: InstanceState
initial = InstanceState [] 0

applyEvents
    :: forall m n a.
       ( MonadState InstanceState m
       , MonadWriter Hooks m
       , MonadReader InstanceState m
       , Functor (Zoomed n Hooks)
       , Zoom n m UnbalancedTxId InstanceState )
    => ContractPrompt Maybe a
    -> m (Maybe a)
applyEvents = flip runPromptTM go . unPlutusContract where
    go hks = do
        hks' <- zoom lastTxId (hooks hks)
        evts <- view events
        case evts of
            []   -> tell hks' >> pure Nothing
            e:es ->
                case match hks e of
                    Nothing -> pure Nothing
                    Just e' -> events .= es >> pure (Just e')

runContract :: forall a. ContractPrompt Maybe a -> InstanceState -> (Maybe a, UnbalancedTxId, Hooks)
runContract con s = over _2 (view lastTxId) $ runRWS (applyEvents con) s s

runContract' :: forall a. ContractPrompt Maybe a -> [Event] -> (Maybe a, Hooks)
runContract' con es =
    let (r, _, h) = runContract con (InstanceState es 0)
    in (r, h)
