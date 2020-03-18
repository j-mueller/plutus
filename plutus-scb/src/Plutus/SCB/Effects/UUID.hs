{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE TemplateHaskell  #-}
module Plutus.SCB.Effects.UUID where

import           Control.Monad.Freer.TH (makeEffect)
import           Eventful.UUID          (UUID)

data UUIDEffect r where
    UuidNextRandom :: UUIDEffect UUID
makeEffect ''UUIDEffect
