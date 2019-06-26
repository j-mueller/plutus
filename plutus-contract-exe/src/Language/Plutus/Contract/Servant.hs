{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeOperators      #-}
module Language.Plutus.Contract.Servant(
      contractServer
    , contractApp
    ) where

import qualified Data.Aeson                        as Aeson
import           Data.Proxy                        (Proxy (..))
import           Servant                           ((:<|>) ((:<|>)), (:>), Get, JSON, Post, ReqBody)
import           Servant.Server                    (Application, Server, serve)

import           GHC.Generics                      (Generic)
import           Language.Plutus.Contract.Contract (ContractPrompt, runContract')
import           Language.Plutus.Contract.Event    (Event)
import           Language.Plutus.Contract.Hooks    (Hooks)

data Call = Call { oldState :: [Event], event :: Event }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (Aeson.FromJSON, Aeson.ToJSON)

data Response = Response { newState :: [Event], hooks :: Hooks }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (Aeson.FromJSON, Aeson.ToJSON)

type ContractAPI =
       "initialise" :> Get '[JSON] Response
  :<|> "run" :> ReqBody '[JSON] Call :> Post '[JSON] Response

-- | Serve a 'PlutusContract' via the contract API
contractServer :: ContractPrompt (Either Hooks) () -> Server ContractAPI
contractServer c = initialise :<|> run where
    initialise = pure (snd (runContract' c []))
    run        = pure . snd . runContract' c

-- | A servant 'Application' that serves a Plutus contract
contractApp :: ContractPrompt (Either Hooks) () -> Application
contractApp = serve (Proxy @ContractAPI) . contractServer
