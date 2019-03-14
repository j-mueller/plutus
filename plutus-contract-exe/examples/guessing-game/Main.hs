{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeOperators              #-}
-- | Contract interface for the guessing game
module Main where

import qualified Data.Aeson                                    as Aeson
import           Data.Proxy                                    (Proxy (Proxy))
import           Data.Text                                     (Text)
import           GHC.Generics                                  (Generic)
import           Network.Wai.Handler.Warp                      (run)
import           Servant                                       ((:<|>) ((:<|>)), (:>), Get, JSON, Post, ReqBody)
import           Servant.Server                                (Application, Server, layout, serve)

import           Language.Plutus.Contract                      (ContractOut (ContractError, StartWatching),
                                                                LedgerUpdate)
import           Language.PlutusTx.Coordination.Contracts.Game (gameAddress)
import           Ledger.Ada                                    (Ada)

-- | Parameters for the "lock" endpoint
data LockParams = LockParams
    { secretWord :: Text
    , amount     :: Ada
    }
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (Aeson.FromJSON, Aeson.ToJSON)

--  | Parameters for the "guess" endpoint
newtype GuessParams = GuessParams
    { guess :: Text
    }
    deriving stock (Eq, Ord, Show, Generic)
    deriving newtype (Aeson.FromJSON, Aeson.ToJSON)

type GuessingGameAPI =

  --  The first two endpoints are the same for all contracts:

  -- ledger-update, informing the contract about changes to the ledger state
    "ledger-update" :> ReqBody '[JSON] LedgerUpdate :> Post '[JSON] [ContractOut] -- POST /ledger-update

  -- initialise, a sequence of 'ContractOut' events that need to be processed
  -- when the contract is first started.
    :<|> "initialise" :> Get '[JSON] [ContractOut]

  -- The following two endpoints are specific to this example (guessing game)

  -- lock some
    :<|> "lock" :> ReqBody '[JSON] LockParams :> Post '[JSON] [ContractOut] -- POST /lock
    :<|> "guess" :> ReqBody '[JSON] GuessParams :> Post '[JSON] [ContractOut] -- POST /guess

    -- returns a textual description of the API (this is only a stand-in until
    -- we have an actual schema endpoint)
    :<|> "layout" :> Get '[JSON] Text

server :: Server GuessingGameAPI
server = ledgerUpdate :<|> initialise :<|> lock :<|> guess_ :<|> l
    where
        ledgerUpdate _ = pure [ContractError "not implemented"]
        initialise     = pure [StartWatching gameAddress]
        lock _         = pure [ContractError "not implemnted"]
        guess_ _       = pure [ContractError "not implemented"]
        l = pure (layout (Proxy @GuessingGameAPI))

app :: Application
app = serve (Proxy @GuessingGameAPI) server

-- Run the server on port 8080
main :: IO ()
main = run 8080 app
