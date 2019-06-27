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

import           Data.Proxy                         (Proxy (..))
import           Servant                            ((:<|>) ((:<|>)), (:>), Get, JSON, Post, ReqBody)
import           Servant.Server                     (Application, Server, serve)
import           GHC.Generics                       (Generic)
import qualified Data.Aeson                         as Aeson

import           Data.Map                           (Map)
import qualified Data.Map                           as Map
import           Language.Plutus.Contract.Contract  (ContractPrompt, runContract')
import           Language.Plutus.Contract.Event     (Event)
import           Language.Plutus.Contract.Hooks     (Hooks)
import           Language.Plutus.Contract.Record
import           Language.Plutus.Contract.RequestId (RequestId)

newtype State = State { record :: Record Event }
    deriving stock (Eq, Show, Generic)
    deriving newtype (Aeson.FromJSON, Aeson.ToJSON)

data Request = Request
    { oldState   :: State
    , event      :: Event
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (Aeson.FromJSON, Aeson.ToJSON)

data Response = Response
    { newState :: State
    , hooks    :: Hooks
    -- , response :: -- user response / status
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (Aeson.FromJSON, Aeson.ToJSON)


type ContractAPI =
       "initialise" :> Get '[JSON] Response
  :<|> "run" :> ReqBody '[JSON] Request :> Post '[JSON] Response

-- | Serve a 'PlutusContract' via the contract API
contractServer :: ContractPrompt (Either Hooks) () -> Server ContractAPI
contractServer c = initialise :<|> run where
    initialise = undefined -- pure . Response .   -- pure (snd (runContract' c []))
    run        = undefined -- pure . snd . runContract' c

-- | A servant 'Application' that serves a Plutus contract
contractApp :: ContractPrompt (Either Hooks) () -> Application
contractApp = serve (Proxy @ContractAPI) . contractServer
