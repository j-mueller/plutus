{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE TemplateHaskell  #-}
module Plutus.SCB.Effects.Contract(
    ContractCommand(..),
    ContractEffect(..),
    invokeContract
    ) where

import           Control.Monad.Freer.TH (makeEffect)
import qualified Data.Aeson             as JSON

import           Plutus.SCB.Types       (PartiallyDecodedResponse)

data ContractCommand
    = InitContract FilePath
    | UpdateContract FilePath JSON.Value
    deriving (Show, Eq)

data ContractEffect r where
    InvokeContract :: ContractCommand -> ContractEffect PartiallyDecodedResponse
makeEffect ''ContractEffect

-- handleContractEffect :: Either SCBError
