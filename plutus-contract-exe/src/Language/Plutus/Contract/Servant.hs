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

import           Language.Plutus.Contract          (PlutusContract)
import           Language.Plutus.Contract.Contract (drain, applyInputs)
import           Language.Plutus.Contract.Event    (Event)
import           Language.Plutus.Contract.Step     (Step, fromBalanced)

type ContractAPI =
       "initialise" :> Get '[JSON] Step
  :<|> "run" :> ReqBody '[JSON] [Event] :> Post '[JSON] Step

-- | Serve a 'PlutusContract' via the contract API
contractServer :: PlutusContract () -> Server ContractAPI
contractServer c = initialise :<|> run where
    initialise = run []
    run es     = pure . fromBalanced . fst . drain $ applyInputs es c

-- | A servant 'Application' that serves a Plutus contract
contractApp :: PlutusContract () -> Application
contractApp = serve (Proxy @ContractAPI) . contractServer
