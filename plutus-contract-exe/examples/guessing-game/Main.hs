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
import qualified Wallet.Emulator.AddressMap                    as AM

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

newtype GameState = GameState
    { interestingAddresses :: AM.AddressMap
    }
    deriving stock (Show, Generic)
    deriving newtype (Aeson.FromJSON, Aeson.ToJSON)

initialState :: GameState
initialState = GameState mempty

type GuessingGameAPI =

  --  All endpoints (except layout) are POST endpoints. They expect the current
  --  'GameState' and an endpoint-specific argument, and return the new
  --  'GameState' and a list of 'ContractOut' events.

  --  The first two endpoints are the same for all contracts:

  -- ledger-update, informing the contract about changes to the ledger state
    "ledger-update" :> ReqBody '[JSON] (GameState, LedgerUpdate) :> Post '[JSON] (GameState, [ContractOut]) -- POST /ledger-update

  -- initialise, a sequence of 'ContractOut' events that need to be processed
  -- when the contract is first started.
    :<|> "initialise" :> Get '[JSON] (GameState, [ContractOut])

  -- The following two endpoints are specific to this example (guessing game)

  -- lock some funds
    :<|> "lock" :> ReqBody '[JSON] (GameState, LockParams) :> Post '[JSON] (GameState, [ContractOut]) -- POST /lock

  -- make a guess
    :<|> "guess" :> ReqBody '[JSON] (GameState, GuessParams) :> Post '[JSON] (GameState, [ContractOut]) -- POST /guess

  -- returns a textual description of the API (this is only a stand-in until
  -- we have an actual schema endpoint)
    :<|> "layout" :> Get '[JSON] Text

server :: Server GuessingGameAPI
server = ledgerUpdate :<|> initialise :<|> lock :<|> guess_ :<|> l
    where
        ledgerUpdate (s, _) = pure (s, [ContractError "not implemented"])
        initialise          = pure (initialState, [StartWatching gameAddress])
        lock (s, _)         = pure (s, [ContractError "not implemented"])
        guess_ (s, _)       = pure (s, [ContractError "not implemented"])
        l = pure (layout (Proxy @GuessingGameAPI))

app :: Application
app = serve (Proxy @GuessingGameAPI) server

-- Run the server on port 8080
main :: IO ()
main = run 8080 app
