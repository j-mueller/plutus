{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators    #-}
module Language.Plutus.Contract.Servant(
      contractServer
    , contractApp
    ) where

import           Data.Proxy                        (Proxy (..))
import           Servant                           ((:<|>) ((:<|>)), (:>), Get, JSON, Post, ReqBody)
import           Servant.Server                    (Application, Server, serve)

import           Language.Plutus.Contract.Contract (Contract, drain, applyInputs)
import           Language.Plutus.Contract.Event    (Event)
import           Language.Plutus.Contract.Hooks    (BalancedHooks, Hooks, fromBalanced)

type ContractAPI =
       "initialise" :> Get '[JSON] Hooks
  :<|> "run" :> ReqBody '[JSON] [Event] :> Post '[JSON] Hooks

-- | Serve a 'PlutusContract' via the contract API
contractServer :: Contract Event BalancedHooks () -> Server ContractAPI
contractServer c = initialise :<|> run where
    initialise = run []
    run es     = pure . fromBalanced . fst . drain $ applyInputs es c

-- | A servant 'Application' that serves a Plutus contract
contractApp :: Contract Event BalancedHooks () -> Application
contractApp = serve (Proxy @ContractAPI) . contractServer
