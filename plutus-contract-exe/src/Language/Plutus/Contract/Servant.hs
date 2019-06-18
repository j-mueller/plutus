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

import           Language.Plutus.Contract.Contract (ContractPrompt, runContract')
import           Language.Plutus.Contract.Event    (Event)
import           Language.Plutus.Contract.Hooks    (Hooks)

type ContractAPI =
       "initialise" :> Get '[JSON] Hooks
  :<|> "run" :> ReqBody '[JSON] [Event] :> Post '[JSON] Hooks

-- | Serve a 'PlutusContract' via the contract API
contractServer :: ContractPrompt Maybe () -> Server ContractAPI
contractServer c = initialise :<|> run where
    initialise = pure (snd (runContract' c []))
    run        = pure . snd . runContract' c

-- | A servant 'Application' that serves a Plutus contract
contractApp :: ContractPrompt Maybe () -> Application
contractApp = serve (Proxy @ContractAPI) . contractServer
