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

import           Control.Monad.Writer
import qualified Data.Aeson                         as Aeson
import           Data.Bifunctor
import           Data.Proxy                         (Proxy (..))
import           GHC.Generics                       (Generic)
import           Servant                            ((:<|>) ((:<|>)), (:>), Get, JSON, Post, ReqBody)
import           Servant.Server                     (Application, Server, serve)

import           Language.Plutus.Contract.Event     (Event)
import           Language.Plutus.Contract.Hooks     (Hooks)
import           Language.Plutus.Contract.Record
import           Language.Plutus.Contract.State     (StatefulContract)
import qualified Language.Plutus.Contract.State     as State

newtype State = State { record :: Record Event }
    deriving stock (Eq, Show, Generic)
    deriving newtype (Aeson.FromJSON, Aeson.ToJSON)

data Request = Request
    { oldState :: State
    , event    :: Event
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
contractServer :: StatefulContract () -> Server ContractAPI
contractServer con = initialise :<|> run where
    initialise = pure (initialResponse con)
    run (Request o e) =
        case State.insertAndUpdate con (record o) e of
            Left err -> error err -- TODO: 404
            Right (r, h) -> pure $ Response (State r) h

-- | A servant 'Application' that serves a Plutus contract
contractApp :: StatefulContract () -> Application
contractApp = serve (Proxy @ContractAPI) . contractServer

initialResponse :: StatefulContract () -> Response
initialResponse =
    uncurry Response
    . first (State . fmap fst)
    . runWriter
    . State.initialise
